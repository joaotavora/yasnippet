;;; yas-doc-helper.el --- Help generate documentation for YASnippet

;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience

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

;; Some functions to help generate YASnippet docs

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org)
(or (require 'org-publish nil t)
    (require 'ox-publish))
(require 'yasnippet) ; docstrings must be loaded

(defun yas--org-raw-html (tag content)
  ;; in version 8.0 org-mode changed the export syntax, see
  ;; http://orgmode.org/worg/org-8.0.html#sec-8-1
  (format (if (version< org-version "8.0.0")
              "@<%s>%s@</%s>"                ; old: @<tag>
            "@@html:<%s>@@%s@@html:</%s>@@") ; new: @@html:<tag>@@
          tag content tag))

(defun yas--document-symbol (symbol level)
  (let* ((stars (make-string level ?*))
         (args (and (fboundp symbol)
                    (mapcar #'symbol-name (help-function-arglist symbol t))))
         (heading (cond ((fboundp symbol)
                         (format
                          "%s =%s= (%s)" stars symbol
                          (mapconcat (lambda (a)
                                       (format (if (string-prefix-p "&" a)
                                                   "/%s/" "=%s=") a))
                                     args " ")))
                        (t
                         (format "%s =%s=\n" stars symbol))))
         (after-heading (format ":PROPERTIES:\n:CUSTOM_ID: %s\n:END:" symbol))
         (body (or (cond ((fboundp symbol)
                          (let ((doc-synth (car-safe (get symbol 'function-documentation))))
                            (if (functionp doc-synth)
                                (funcall doc-synth nil)
                              (documentation symbol t))))
                         ((boundp symbol)
                          (documentation-property symbol 'variable-documentation t))
                         (t
                          (format "*WARNING*: no symbol named =%s=" symbol)))
                   (format "*WARNING*: no doc for symbol =%s=" symbol)))
         (case-fold-search nil))
    ;; do some transformations on the body:
    ;; ARGxxx becomes @<code>arg@</code>xxx
    ;; FOO becomes /foo/
    ;; `bar' becomes [[#bar][=bar=]]
    (setq body (replace-regexp-in-string
                "\\<\\([A-Z][-A-Z0-9]+\\)\\(\\sw+\\)?\\>"
                #'(lambda (match)
                    (let* ((match1 (match-string 1 match))
                           (prefix (downcase match1))
                           (suffix (match-string 2 match))
                           (fmt (cond
                                 ((member prefix args)
                                  (yas--org-raw-html "code" "%s"))
                                 ((null suffix) "/%s/"))))
                      (if fmt (format fmt prefix)
                        match1)))
                body t t 1)
          body (replace-regexp-in-string
                "`\\([a-z-]+\\)'"
                #'(lambda (match)
                    (let* ((name (downcase (match-string 1 match)))
                           (sym (intern name)))
                      (if (memq sym yas--exported-syms)
                          (format "[[#%s][=%s=]]" name name)
                        (format "=%s=" name))))
                body t))
    ;; output the paragraph
    (concat heading "\n" after-heading "\n" body)))

(defun yas--document-symbols (level &rest names-and-predicates)
  (let ((sym-lists (make-vector (length names-and-predicates) nil))
        (stars (make-string level ?*)))
    (loop for sym in yas--exported-syms
          do (loop for test in (mapcar #'cdr names-and-predicates)
                   for i from 0
                   do (when (funcall test sym)
                        (push sym (aref sym-lists i))
                        (return))))
    (loop for slist across sym-lists
          for name in (mapcar #'car names-and-predicates)
          concat (format "\n%s %s\n" stars name)
          concat (mapconcat (lambda (sym)
                              (yas--document-symbol sym (1+ level)))
                            slist "\n\n"))))

(defun yas--internal-link-snippet ()
  (interactive)
  (yas-expand-snippet "[[#$1][=${1:`yas/selected-text`}=]]"))

(define-key org-mode-map [M-f8] 'yas--internal-link-snippet)

;; This lets all the org files be exported to HTML with
;; `org-publish-current-project' (C-c C-e P).

(let* ((dir (if load-file-name (file-name-directory load-file-name)
              default-directory))
       (src-epoch (getenv "SOURCE_DATE_EPOCH"))
       ;; Presence of SOURCE_DATE_EPOCH indicates a reproducible
       ;; build, don't depend on git.
       (rev (unless src-epoch
              (ignore-errors
                (car (process-lines "git" "describe" "--dirty")))))
       (date (format-time-string
              "(%Y-%m-%d %H:%M:%S)"
              (seconds-to-time
               (string-to-number
                (or (if rev (car (process-lines "git" "show" "--format=%ct"))
                      src-epoch)
                    "0")))
              t))
       (proj-plist
        `(,@(when (fboundp 'org-html-publish-to-html)
              '(:publishing-function org-html-publish-to-html))
          :base-directory ,dir :publishing-directory ,dir
          :html-preamble
          ,(with-temp-buffer
             (insert-file-contents (expand-file-name "nav-menu.html.inc" dir))
             (buffer-string))
          :html-postamble
          ,(concat "<hr><p class='creator'>Generated by %c from "
                   (or rev yas--version) " " date "</p>\n"
                   "<p class='xhtml-validation'>%v</p>\n")))
       (project (assoc "yasnippet" org-publish-project-alist)))
  (when rev ;; Rakefile :doc:upload uses "html-revision".
    (with-temp-file (expand-file-name "html-revision" dir)
      (princ rev (current-buffer))))
  (if project
      (setcdr project proj-plist)
    (push `("yasnippet" . ,proj-plist)
          org-publish-project-alist)))

(defun yas--generate-html-batch ()
  (let ((org-publish-use-timestamps-flag nil)
        (org-export-copy-to-kill-ring nil)
        (org-confirm-babel-evaluate nil)
        (make-backup-files nil)
        (org-html-htmlize-output-type 'css))
    (org-publish "yasnippet" 'force)))



(provide 'yas-doc-helper)
;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;; yas-doc-helper.el ends here
