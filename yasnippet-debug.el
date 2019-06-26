;;; yasnippet-debug.el --- debug functions for yasnippet -*- lexical-binding: t -*-

;; Copyright (C) 2010, 2013-2014, 2017-2018  Free Software Foundation, Inc.

;; Author: João Távora
;; Keywords: emulations, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some debug functions.  When loaded from the command line, provides
;; quick way to test out snippets in a fresh Emacs instance.
;;
;; emacs -Q -l yasnippet-debug [-v[v]]
;;     [-M:<modename>] [-M.<filext>] [-S:[<snippet-file|name>]]
;;     [-- <more-arguments-passed-to-Emacs>...]
;;
;; See the source in `yas-debug-process-command-line' for meaning of
;; args.
;;
;;; Code:

(defconst yas--loaddir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that yasnippet was loaded from.")

(require 'yasnippet (if (boundp 'yas--loaddir)
                        ;; Don't require '-L <path>' when debugging.
                        (expand-file-name "yasnippet" yas--loaddir)))
(require 'cl-lib)
(eval-when-compile
  (unless (fboundp 'cl-flet)
    (defalias 'cl-flet 'flet)))
(require 'color nil t)
(require 'edebug)
(eval-when-compile
  (require 'subr-x nil t)
  (cond ((fboundp 'when-let*) nil) ; Introduced in 26.
        ((fboundp 'when-let)       ; Introduced in 25.1,
         (defalias 'when-let* 'when-let)) ; deprecated in 26.
        (t (defmacro when-let* (key-vals &rest body)
             (declare (indent 1) (debug ((symbolp form) body)))
             (let ((key-val (pop key-vals)))
               (if key-val
                   `(let ((,(car key-val) ,(cadr key-val)))
                      (if ,(car key-val)
                        (when-let* ,key-vals
                          ,@body)))
                 `(progn ,@body)))))))

(defvar yas-debug-live-indicators
  (make-hash-table :test #'eq))

(defun yas-debug-live-colors ()
  (let ((colors ()))
    (maphash (lambda (_k v) (push (nth 1 (car v)) colors)) yas-debug-live-indicators)
    colors))

(defvar yas-debug-recently-live-indicators)

(defun yas-debug-get-live-indicator (location)
  (require 'color)
  (when (boundp 'yas-debug-recently-live-indicators)
    (push location yas-debug-recently-live-indicators))
  (let (beg end)
    (if (markerp location)
        (setq beg (setq end (marker-position location)))
      (setq beg (yas-debug-ov-fom-start location)
            end (yas-debug-ov-fom-end location)))
    (or (when-let* ((color-ov (gethash location yas-debug-live-indicators)))
          (if (and beg end) (move-overlay (cdr color-ov) beg end)
            (delete-overlay (cdr color-ov)))
          color-ov)
        (let* ((live-colors (yas-debug-live-colors))
               (color
                (cl-loop with best-color = nil with max-dist = -1
                         for color = (format "#%06X" (random #x1000000))
                         for comp = (if (fboundp 'color-complement)
                                        (apply #'color-rgb-to-hex (color-complement color))
                                      color)
                         if (< (color-distance color (face-foreground 'default))
                               (color-distance comp (face-foreground 'default)))
                         do (setq color comp)
                         for dist = (cl-loop for c in live-colors
                                             minimize (color-distance c color))
                         if (or (not live-colors) (> dist max-dist))
                         do (setq best-color color) (setq max-dist dist)
                         repeat (if live-colors 100 1)
                         finally return `(:background ,best-color)))
               (ov (make-overlay beg end)))
          (if (markerp location)
              (overlay-put ov 'before-string (propertize "↓" 'face color))
            (overlay-put ov 'before-string (propertize "↘" 'face color))
            (overlay-put ov 'after-string (propertize "↙" 'face color)))
          (puthash location (cons color ov) yas-debug-live-indicators)))))

(defun yas-debug-live-marker (marker)
  (let* ((color-ov (yas-debug-get-live-indicator marker))
         (color (car color-ov))
         (ov (cdr color-ov))
         (decorator (overlay-get ov 'before-string))
         (str (format "at %d" (+ marker))))
    (if (markerp marker)
        (propertize str
                    'cursor-sensor-functions
                    `(,(lambda (_window _oldpos dir)
                         (overlay-put
                          ov 'before-string
                          (propertize decorator
                                      'face (if (eq dir 'entered)
                                                'mode-line-highlight color)))))
                    'face color)
      str)))

(defun yas-debug-ov-fom-start (ovfom)
  (cond ((overlayp ovfom) (overlay-start ovfom))
        ((integerp ovfom) ovfom)
        (t (yas--fom-start ovfom))))
(defun yas-debug-ov-fom-end (ovfom)
  (cond ((overlayp ovfom) (overlay-end ovfom))
        ((integerp ovfom) ovfom)
        (t (yas--fom-end ovfom))))

(defun yas-debug-live-range (range)
  (let* ((color-ov (yas-debug-get-live-indicator range))
         (color (car color-ov))
         (ov (cdr color-ov))
         (decorator-beg (overlay-get ov 'before-string))
         (decorator-end (overlay-get ov 'after-string))
         (beg (yas-debug-ov-fom-start range))
         (end (yas-debug-ov-fom-end range)))
    (if (and beg end (or (overlayp range)
                         (and (not (integerp beg))
                              (not (integerp end)))))
        (propertize (format "from %d to %d" (+ beg) (+ end))
                    'cursor-sensor-functions
                    `(,(lambda (_window _oldpos dir)
                         (let ((face (if (eq dir 'entered)
                                         'mode-line-highlight color)))
                           (overlay-put ov 'before-string
                                        (propertize decorator-beg 'face face))
                           (overlay-put ov 'after-string
                                        (propertize decorator-end 'face face)))))
                    'face color)
      "<dead>")))

(defmacro yas-debug-with-tracebuf (outbuf &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((tracebuf-var (make-symbol "tracebuf")))
    `(let ((,tracebuf-var (or ,outbuf (get-buffer-create "*YASnippet trace*"))))
       (unless (eq ,tracebuf-var (current-buffer))
         (cl-flet ((printf (fmt &rest args)
                           (with-current-buffer ,tracebuf-var
                             (insert (apply #'format fmt args)))))
           (unless ,outbuf
             (with-current-buffer ,tracebuf-var
               (erase-buffer)
               (when (fboundp 'cursor-sensor-mode)
                 (cursor-sensor-mode +1))
               (setq truncate-lines t)))
           (setq ,outbuf ,tracebuf-var)
           (save-restriction
             (widen)
             ,@body))))))


(defun yas-debug-snippet (snippet &optional outbuf)
  (yas-debug-with-tracebuf outbuf
    (when-let* ((overlay (yas--snippet-control-overlay snippet)))
      (printf "\tsid: %d control overlay %s\n"
              (yas--snippet-id snippet)
              (yas-debug-live-range overlay)))
    (when-let* ((active-field (yas--snippet-active-field snippet)))
      (unless (consp (yas--field-start active-field))
        (printf "\tactive field: #%d %s %s covering \"%s\"\n"
                (or (yas--field-number active-field) -1)
                (if (yas--field-modified-p active-field) "**" "--")
                (yas-debug-live-range active-field)
                (buffer-substring-no-properties (yas--field-start active-field) (yas--field-end active-field)))))
    (when-let* ((exit (yas--snippet-exit snippet)))
      (printf "\tsnippet-exit: %s next: %s\n"
              (yas-debug-live-marker (yas--exit-marker exit))
              (yas--exit-next exit)))
    (dolist (field (yas--snippet-fields snippet))
      (unless (consp (yas--field-start field))
        (printf "\tfield: %d %s %s covering \"%s\" next: %s%s\n"
                (or (yas--field-number field) -1)
                (if (yas--field-modified-p field) "**" "--")
                (yas-debug-live-range field)
                (buffer-substring-no-properties (yas--field-start field) (yas--field-end field))
                (yas--debug-format-fom-concise (yas--field-next field))
                (if (yas--field-parent-field field)
                    (format " parent: %s"
                            (yas--debug-format-fom-concise
                             (yas--field-parent-field field)))
                  "")))
      (dolist (mirror (yas--field-mirrors field))
        (unless (consp (yas--mirror-start mirror))
          (printf "\t\tmirror: %s covering \"%s\" next: %s\n"
                  (yas-debug-live-range mirror)
                  (buffer-substring-no-properties (yas--mirror-start mirror) (yas--mirror-end mirror))
                  (yas--debug-format-fom-concise (yas--mirror-next mirror))))))))

(defvar yas-debug-target-buffer nil)
(defvar yas-debug-target-snippets nil nil)
(make-variable-buffer-local 'yas-debug-target-snippets)

(defvar yas-debug-undo nil)

(defun yas-toggle-debug-undo (value)
  (interactive (list (not yas-debug-undo)))
  (setq yas-debug-undo value)
  (yas--message 3 "debug undo %sabled" (if yas-debug-undo "en" "dis")))

(defadvice yas--snippet-parse-create (before yas-debug-target-snippet (snippet))
  (add-to-list 'yas-debug-target-snippets snippet))

(defadvice yas--commit-snippet (after yas-debug-untarget-snippet (snippet))
  (setq yas-debug-target-snippets
        (remq snippet yas-debug-target-snippets))
  (maphash (lambda (k color-ov)
             (delete-overlay (cdr color-ov)))
           yas-debug-live-indicators)
  (clrhash yas-debug-live-indicators))

(defun yas-debug-snippets (&optional outbuf hook)
  "Print debug information on active snippets to buffer OUTBUF.
If OUTBUF is nil, use a buffer named \"*YASsnippet trace*\".
If HOOK is non-nil, install `yas-debug-snippets' in
`post-command-hook' to update the information on every command
after this one. If it is `snippet-navigation' then install hook
buffer-locally, otherwise install it globally.  If HOOK is
`edebug-create', also instrument the function
`yas--snippet-parse-create' with `edebug' and show its source."
  (interactive (list nil t))
  (condition-case err
      (yas-debug-with-tracebuf outbuf
        (unless (buffer-live-p yas-debug-target-buffer)
          (setq yas-debug-target-buffer nil))
        (with-current-buffer (or yas-debug-target-buffer (current-buffer))
          (when yas-debug-target-snippets
            (setq yas-debug-target-snippets
                  (cl-delete-if-not #'yas--snippet-p yas-debug-target-snippets)))
          (let ((yas-debug-recently-live-indicators nil))
            (printf "(length yas--snippets-snippets) => %d\n"
                    (length yas--active-snippets))
            (dolist (snippet (or yas-debug-target-snippets
                                 (yas-active-snippets)))
              (printf "snippet %d\n" (yas--snippet-id snippet))
              (yas-debug-snippet snippet outbuf))
            (maphash (lambda (loc color-ov)
                       (unless (memq loc yas-debug-recently-live-indicators)
                         (delete-overlay (cdr color-ov))
                         (remhash loc yas-debug-live-indicators)))
                     yas-debug-live-indicators))
          (when (and yas-debug-undo (listp buffer-undo-list))
            (printf "Undo list has %s elements:\n" (length buffer-undo-list))
            (cl-loop for undo-elem in buffer-undo-list
                     do (printf "%S\n" undo-elem))))
        (when hook
          (setq yas-debug-target-buffer (current-buffer))
          (ad-enable-advice 'yas--snippet-parse-create 'before 'yas-debug-target-snippet)
          (ad-activate 'yas--snippet-parse-create)
          (ad-enable-advice 'yas--commit-snippet 'after 'yas-debug-untarget-snippet)
          (ad-activate 'yas--commit-snippet)
          (add-hook 'post-command-hook #'yas-debug-snippets
                    nil (eq hook 'snippet-navigation))
          ;; Window management is slapped together, it does what I
          ;; want when the caller has a single window open.  Good
          ;; enough for now.
          (when (eq hook 'edebug-create)
            (edebug-instrument-function 'yas--snippet-parse-create)
            (let ((buf-point (find-function-noselect 'yas--snippet-parse-create)))
              (with-current-buffer (car buf-point)
                (goto-char (cdr buf-point)))))
          outbuf))
    ((debug error) (signal (car err) (cdr err)))))

(defun yas-debug-snippet-create ()
  (yas-debug-snippets nil 'create))

(defun yas--debug-format-fom-concise (fom)
  (when fom
    (cond ((yas--field-p fom)
           (format "field %s from %d to %d"
                   (yas--field-number fom)
                   (+ (yas--field-start fom))
                   (+ (yas--field-end fom))))
          ((yas--mirror-p fom)
           (format "mirror from %d to %d"
                   (+ (yas--mirror-start fom))
                   (+ (yas--mirror-end fom))))
          (t
           (format "snippet exit at %d"
                   (+ (yas--fom-start fom)))))))

(defun yas-debug-process-command-line (&optional options)
  "Implement command line processing."
  (setq yas-verbosity 99)
  (setq yas-triggers-in-field t)
  (setq debug-on-error t)
  (let* ((snippet-mode 'fundamental-mode)
         (snippet-key nil))
    (unless options
      (setq options (cl-loop for opt = (pop command-line-args-left)
                             while (and opt (not (equal opt "--"))
                                        (string-prefix-p "-" opt))
                             collect opt)))
    (when-let* ((mode (cl-member "-M:" options :test #'string-prefix-p)))
      (setq snippet-mode (intern (concat (substring (car mode) 3) "-mode"))))
    (when-let* ((mode (cl-member "-M." options :test #'string-prefix-p)))
      (setq snippet-mode
            (cdr (cl-assoc (substring (car mode) 2) auto-mode-alist
                           :test (lambda (ext regexp) (string-match-p regexp ext))))))
    (switch-to-buffer (get-buffer-create "*yas test*"))
    (funcall snippet-mode)
    (when-let* ((snippet-file (cl-member "-S:" options :test #'string-prefix-p)))
      (setq snippet-file (substring (car snippet-file) 3))
      (if (file-exists-p snippet-file)
          (with-temp-buffer
            (insert-file-contents snippet-file)
            (let ((snippet-deflist (yas--parse-template snippet-file)))
              (yas-define-snippets snippet-mode (list snippet-deflist))
              (setq snippet-key (car snippet-deflist))))
        (yas-reload-all)
        (let ((template (yas--lookup-snippet-1 snippet-file snippet-mode)))
          (if template
              (setq snippet-key (yas--template-key template))
            (error "No such snippet `%s'" snippet-file)))))
    (display-buffer (find-file-noselect
                     (expand-file-name "yasnippet.el" yas--loaddir)))
    (when-let* ((verbosity (car (or (member "-v" options) (member "-vv" options)))))
      (set-window-buffer
       (split-window) (yas-debug-snippets
                       nil (if (equal verbosity "-vv") 'edebug-create t))))
    (yas-minor-mode +1)
    (when snippet-key (insert snippet-key))))

(when command-line-args-left
  (yas-debug-process-command-line))

(provide 'yasnippet-debug)
;; Local Variables:
;; indent-tabs-mode: nil
;; autoload-compute-prefixes: nil
;; End:
;;; yasnippet-debug.el ends here
