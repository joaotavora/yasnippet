;;; snippet.el --- yasnippet's engine distilled  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  João Távora

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

;;

;;; Code:

(require 'cl-lib)


;;; the define-snippet macro and its helpers
;;;
(defun snippet--form-make-field-sym (field-name &optional parent-field-sym)
  (make-symbol (format "field-%s%s" field-name
                       (if parent-field-sym
                           (format "-son-of-%s" parent-field-sym)
                         ""))))
(defun snippet--form-make-mirror-sym (mirror-name source-field-name
                                                  &optional parent-field-sym)
  (make-symbol (format "mirror-%s-of-%s%s" mirror-name source-field-name
                       (if parent-field-sym
                           (format "-son-of-%s" parent-field-sym)
                         ""))))

(defvar snippet--marker-sym-obarray (make-vector 100 nil))

(defun snippet--start-marker-name (sym)
  (intern (format "%s-beg" sym) snippet--marker-sym-obarray))

(defun snippet--end-marker-name (sym)
  (intern (format "%s-end" sym) snippet--marker-sym-obarray))

(defvar snippet--form-mirror-sym-idx nil)

(defun snippet--function-p (form)
  (or (functionp form)
      (and (eq 'function (car form))
           (fboundp (cl-second form)))
      (and (eq 'quote (car form))
           (fboundp (cl-second form)))))

(defun snippet--form-sym-tuples (forms &optional parent-field-sym)
  "Produce information for composing the snippet expansion function.

A tuple of 6 elements is created for each form in FORMS.

\(SYM FORM PARENT-FIELD-SYM ADJACENT-PREV-SYM PREV-FORM NEXT-FORM)

Forms representing fields with nested elements are recursively
iterated depth-first, resulting in a flattened list."
  (cl-loop unless forms return nil
           with snippet--form-mirror-sym-idx = (or snippet--form-mirror-sym-idx
                                                   0)
           with adjacent-prev-sym

           for (prev-form form next-form) on `(nil ,@forms)

           for (sym childrenp) = (pcase form
                                   (`(field ,name . ,rest)
                                    (list (snippet--form-make-field-sym
                                           name
                                           parent-field-sym)
                                          (listp (car rest))))
                                   (`(mirror ,name . ,_)
                                    (incf snippet--form-mirror-sym-idx)
                                    (list (snippet--form-make-mirror-sym
                                           snippet--form-mirror-sym-idx
                                           name
                                           parent-field-sym))))

           append (cond (sym
                         `((,sym
                            ,form
                            ,parent-field-sym
                            ,adjacent-prev-sym
                            ,prev-form
                            ,next-form)
                           ,@(when childrenp
                               (snippet--form-sym-tuples (third form) sym))))
                        ((null form) nil)
                        ((or (stringp form)
                             (snippet--function-p form))
                         `((string-or-function ,form ,parent-field-sym)))
                        (t
                         (error "unknown type of snippet form %s" form)))
           do (setq adjacent-prev-sym sym)))

(defun snippet--make-marker-init-forms (tuples)
  "Make marker init forms for the snippet objects in TUPLES.

Imagine this snippet:

 ff1 sss mm1 ff2         mm5
              |
              ff3 sss mm4

I would need these somewhere in the let* form

 ((ff1-beg (make-marker))
  (ff1-end (make-marker))
  (mm1-beg (make-marker))
  (mm1-end (make-marker))
  (ff2-beg mm1-end)
  (ff2-end (make-marker))
  (ff3-beg ff2-end)
  (ff3-end (make-marker))
  (mm4-beg (make-marker))
  (mm4-end ff2-end)
  (mm5-beg ff2-end)
  (mm5-end (make-marker)))
"
  (cl-loop for (sym nil parent-sym adjacent-prev-sym prev next) in tuples
           unless (eq sym 'string-or-function)
           append `((,(snippet--start-marker-name sym)
                     ,(or (and adjacent-prev-sym
                               (snippet--end-marker-name adjacent-prev-sym))
                          (and parent-sym
                               (not prev)
                               (snippet--start-marker-name parent-sym))
                          `(snippet--make-marker)))
                    (,(snippet--end-marker-name sym)
                     ,(or (and parent-sym
                               (not next)
                               (snippet--end-marker-name parent-sym))
                          `(snippet--make-marker))))))


(defun snippet--first-field-sym (tuples)
  (car (car (snippet--field-tuples tuples))))

(defun snippet--field-tuples (tuples)
  (cl-remove-if-not #'(lambda (form)
                        (and (consp form)
                             (eq 'field (car form))))
                    tuples :key #'cadr))

(defun snippet--init-field-and-mirror-forms (tuples)
  (let* ((field-mirrors (make-hash-table))
         ;; we first collect `snippet--make-mirror' forms. When
         ;; collecting them, we populate the `field-mirrors' table...
         ;;
         (tuples (cl-remove 'string-or-function tuples :key #'car))
         (make-mirror-forms
          (cl-loop for (sym (type name transform) parent-sym) in tuples
                   when (eq 'mirror type)
                   collect (let ((source-sym nil))
                             (cl-loop for (sym-b (type-b name-b)) in tuples
                                      when (and
                                            (eq 'field type-b)
                                            (eq name name-b))
                                      do
                                      (setq source-sym sym-b)
                                      (puthash source-sym
                                               (cons sym (gethash source-sym
                                                                  field-mirrors))
                                               field-mirrors))
                             (unless source-sym
                               (error "mirror mentions unknown field %s"
                                      name))
                             `((,sym (snippet--make-mirror))
                               (snippet--init-mirror
                                ,sym
                                ,source-sym
                                ,(snippet--start-marker-name sym)
                                ,(snippet--end-marker-name sym)
                                ,(snippet--transform-lambda transform)
                                ,parent-sym)))))
         ;; so that we can now create `snippet--make-field' forms with
         ;; complete lists of mirror symbols.
         ;;
         (make-field-forms
          (cl-loop with field-tuples = (snippet--field-tuples tuples)
                   for ((prev-sym)
                        (sym (_type name _value) parent-sym)
                        (next-sym)) on `(nil ,@field-tuples)
                        when sym

                        collect `((,sym (snippet--make-field))
                                  (snippet--init-field
                                   ,sym
                                   ,name
                                   ,(snippet--start-marker-name sym)
                                   ,(snippet--end-marker-name sym)
                                   ,parent-sym
                                   (list
                                    ,@(reverse
                                       (gethash sym field-mirrors)))
                                   ,next-sym
                                   ,prev-sym)))))

    (append make-field-forms
            make-mirror-forms)))

(defun snippet--transform-lambda (transform-form)
  `(lambda (field-text)
     ,(or transform-form
          'field-text)))

(defun define--snippet-body (body)
  "Does the actual work for `define-snippet'"
  (let* ((sym-tuples (snippet--form-sym-tuples body))
         (marker-init-forms (snippet--make-marker-init-forms sym-tuples))
         (init-object-forms (snippet--init-field-and-mirror-forms sym-tuples))
         (first-field-sym (snippet--first-field-sym sym-tuples)))
    `(let* (,@(mapcar #'car init-object-forms)
            ,@marker-init-forms)

       ,@(mapcar #'second init-object-forms)

       ,@(cl-loop
          for (sym form) in sym-tuples
          collect (pcase form
                    (`(field ,_ ,text)
                     `(snippet--insert-field ,sym ,(if (stringp text)
                                                       text)))
                    (`(mirror . ,_)
                     `(snippet--insert-mirror ,sym))
                    ((pred stringp)
                     `(insert ,form))
                    ((pred functionp)
                     `(insert (funcall ,form)))))

       (setq snippet--field-overlay
             (make-overlay (point) (point) nil nil t))
       (overlay-put snippet--field-overlay
                    'face
                    'snippet-field-face)
       (overlay-put snippet--field-overlay
                    'modification-hooks
                    '(snippet--field-overlay-changed))
       (overlay-put snippet--field-overlay
                    'insert-in-front-hooks
                    '(snippet--field-overlay-changed))
       (overlay-put snippet--field-overlay
                    'insert-behind-hooks
                    '(snippet--field-overlay-changed))
       (overlay-put snippet--field-overlay
                    'keymap
                    snippet-field-keymap)
       (overlay-put snippet--field-overlay
                    'snippet--objects
                    (list ,@(remove 'string-or-function
                                    (mapcar #'car
                                            sym-tuples))))
       ,(if first-field-sym
            `(snippet--move-to-field ,first-field-sym))
       (add-hook 'post-command-hook 'snippet--post-command-hook t t))))


(cl-defmacro define-snippet (name () &rest body)
  "Define NAME as a snippet-inserting function.

NAME's function definition is set to a function with no arguments
that inserts the fields components at point.

Each form in BODY can be:

* A cons (field FIELD-NAME FIELD-VALUE FIELD-TRANSFORM)
  definining a snippet field. A snippet field can be navigated to
  using `snippet-next-field' and
  `snippet-prev-field'. FIELD-TRANSFORM is currently
  unimplemented.

* A cons (mirror FIELD-NAME MIRROR-TRANSFORM) defining a mirror
  of the field named FIELD-NAME. Each time the text under the
  field changes, the form MIRROR-TRANSFORM is invoked with the
  variable `field-text' set to the text under the field. The
  string produced become the text under the mirror.

* A string literal which is inserted as a literal part of the
  snippet and remains unchanged while the snippet is navigated.

* A symbol designating a function which is called when the
  snippet is inserted. The string produced is treated as a
  literal string.

* A lambda form taking no arguments, called when the snippet is
  inserted. Again, the string produced is treated as a literal
  snippet string.

ARGS is an even-numbered property list of (KEY VAL) pairs. KEY
can be:

* the symbol `:obarray', in which case the symbol NAME in
  interned in the obarray VAL instead of the global obarray. This
  options is currently unimplemented."
  (declare (debug (&define name sexp &rest &or
                           ;; curiously, function-form doesn't work here
                           ;;
                           ("mirror" sexp def-form)
                           ("lambda" sexp def-form)
                           ("field" sexp &rest sexp)
                           sexp)))
  `(defun ,name ()
     ,(define--snippet-body body)))

(cl-defmacro make-snippet (&rest body)
  "Same as `define-snippet', but return an anonymous function."
  `(lambda () ,(define--snippet-body body)))


;;; Snippet mechanics
;;;

(cl-defstruct snippet--object
  start end parent-field next prev (buffer (current-buffer)))

(cl-defstruct (snippet--field (:constructor snippet--make-field ())
                              (:include snippet--object)
                              (:print-function snippet--describe-field))
  name
  (mirrors '())
  next-field
  prev-field)

(defun snippet--describe-field (field)
  (with-current-buffer (snippet--object-buffer field)
    (format "field %s from %s to %s covering \"%s\""
            (snippet--field-name field)
            (marker-position (snippet--object-start field))
            (marker-position (snippet--object-end field))
            (buffer-substring-no-properties
             (snippet--object-start field)
             (snippet--object-end field)))))

(defun snippet--init-field (object name start end parent-field mirrors
                                   next-field prev-field)
  (setf (snippet--field-name object) name
        (snippet--object-start object) start
        (snippet--object-end object) end
        (snippet--object-parent-field object) parent-field
        (snippet--field-mirrors object) mirrors
        (snippet--field-next-field object) next-field
        (snippet--field-prev-field object) prev-field))

(cl-defstruct (snippet--mirror (:constructor snippet--make-mirror ())
                               (:include snippet--object)
                               (:print-function snippet--describe-mirror))
  source
  (transform nil))

(defun snippet--init-mirror (object source start end transform parent-field)
  (setf (snippet--mirror-source object) source
        (snippet--object-start object) start
        (snippet--object-end object) end
        (snippet--mirror-transform object) transform
        (snippet--object-parent-field object) parent-field))

(defun snippet--describe-mirror (mirror)
  (with-current-buffer (snippet--object-buffer mirror)
    (format "mirror from %s to %s covering \"%s\""
            (marker-position (snippet--object-start mirror))
            (marker-position (snippet--object-end mirror))
            (buffer-substring-no-properties
             (snippet--object-start mirror)
             (snippet--object-end mirror)))))

(defgroup snippet nil
  "Customize snippet features"
  :group 'convenience)

(defface snippet-field-face
  '((t (:inherit 'region)))
  "Face used to highlight the currently active field of a snippet")

(defvar snippet-field-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>")       'snippet-next-field)
    (define-key map (kbd "S-<tab>")     'snippet-prev-field)
    map)
  "The active keymap while a snippet expansion is in progress.")

(defvar snippet--field-overlay nil)

(defun snippet-next-field (&optional prev)
  (interactive)
  (let ((field (overlay-get snippet--field-overlay 'snippet--field)))
    (cond (prev
           (if (snippet--field-prev-field field)
               (snippet--move-to-field (snippet--field-prev-field field))
             (goto-char (snippet--object-start field))
             (snippet-exit-snippet)))
          (t
           (if (snippet--field-next-field field)
               (snippet--move-to-field (snippet--field-next-field field))
             (goto-char (snippet--object-end field))
             (snippet-exit-snippet))))))

(defun snippet-prev-field ()
  (interactive)
  (snippet-next-field t))

(defun snippet-exit-snippet (&optional reason)
  (delete-overlay snippet--field-overlay)
  (message "snippet exited%s"
           (or (and reason
                    (format " (%s)" reason))
               "")))

(defun snippet--make-marker ()
  (let ((marker (make-marker)))
    (set-marker-insertion-type marker t)
    (set-marker marker (point))))

(defun snippet--open-markers (start end)
  (set-marker-insertion-type start nil)
  (set-marker-insertion-type end t))

(defun snippet--close-markers (start end)
  (cond ((= start end)
         (set-marker-insertion-type start t)
         (set-marker-insertion-type end t))
        (t
         (set-marker-insertion-type start t)
         (set-marker-insertion-type end nil))))

(defun snippet--call-with-current-object (object fn)
  (let* ((start (snippet--object-start object))
         (end (snippet--object-end object)))
    (unwind-protect
        (progn
          (snippet--open-markers start end)
          (funcall fn))
      (snippet--close-markers start end))))

(defmacro snippet--with-current-object (object &rest body)
  (declare (indent defun) (debug t))
  `(snippet--call-with-current-object ,object #'(lambda () ,@body)))


(defun snippet--insert-field (field text)
  (when text
    (snippet--with-current-object field
      (insert text))))

(defun snippet--insert-mirror (mirror)
  (snippet--update-mirror mirror))

(defun snippet--update-mirror (mirror)
  (snippet--with-current-object mirror
    (delete-region (snippet--object-start mirror)
                   (snippet--object-end mirror))
    (save-excursion
      (goto-char (snippet--object-start mirror))
      (insert (funcall (snippet--mirror-transform mirror)
                       (snippet--field-text (snippet--mirror-source mirror)))))))

(defun snippet--move-to-field (field)
  (goto-char (snippet--object-start field))
  (move-overlay snippet--field-overlay
                (point)
                (snippet--object-end field))
  (overlay-put snippet--field-overlay 'snippet--field field))

(defun snippet--field-overlay-changed (overlay after? _beg _end &optional _length)
  (let* ((field (overlay-get overlay 'snippet--field))
         (inhibit-modification-hooks t))
    (cond (after?
           (snippet--close-markers (snippet--object-start field)
                                   (snippet--object-end field))
           (mapc #'snippet--update-mirror (snippet--field-mirrors field))
           (move-overlay overlay
                         (snippet--object-start field)
                         (snippet--object-end field)))
          (t
           (snippet--open-markers (snippet--object-start field)
                                  (snippet--object-end field))))))

(defun snippet--field-text (field)
  (buffer-substring-no-properties (snippet--object-start field)
                                  (snippet--object-end field)))

(defvar snippet--debug nil)
;; (setq snippet--debug t)

(defun snippet--post-command-hook ()
  (cond ((and snippet--field-overlay
              (overlay-buffer snippet--field-overlay))
         (cond ((or (< (point)
                       (overlay-start snippet--field-overlay))
                    (> (point)
                       (overlay-end snippet--field-overlay)))
                (snippet-exit-snippet "point left snippet")
                (remove-hook 'post-command-hook 'snippet--post-command-hook t))
               (snippet--debug
                (snippet--debug-snippet snippet--field-overlay))))
        (snippet--field-overlay
         ;; snippet must have been exited for some other reason
         ;;
         (remove-hook 'post-command-hook 'snippet--post-command-hook t))))

(defun snippet--debug-snippet (field-overlay)
  (with-current-buffer (get-buffer-create "*snippet-debug*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((active-field (overlay-get field-overlay 'snippet--field)))
        (cl-loop for object in (overlay-get field-overlay 'snippet--objects)
                 when (snippet--field-p object)
                 do
                 (insert (snippet--describe-field object))
                 (when (eq object active-field) (insert " (active)"))
                 (insert "\n")
                 (cl-loop for mirror in (snippet--field-mirrors object)
                          do (insert "  " (snippet--describe-mirror mirror)
                                     "\n")))))
    (display-buffer (current-buffer))))

(provide 'snippet)

;; Local Variables:
;; coding: utf-8
;; whitespace-style: (face lines-tail)
;; whitespace-line-column: 80
;; fill-column: 80
;; End:
;; snippet.el ends here
