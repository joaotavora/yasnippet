;;; snippet.el --- yasnippet's snippet engine distilled  -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'cl))


(cl-defstruct (snippet--field (:constructor snippet--make-field (name mirrors parent-field start end)))
  name
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)

(cl-defstruct (snippet--mirror (:constructor snippet--make-mirror (source transform parent-field start end)))
  source
  start end
  (transform nil)
  parent-field
  next
  depth)

(defun snippet--make-marker ()
  (let ((marker (make-marker)))
    (set-marker-insertion-type marker t)
    (set-marker marker (point))))

(defmacro snippet--with-current-object (object &rest body)
  (declare (indent defun))
  `(snippet--call-with-current-object ,object #'(lambda () ,@body)))

(defun snippet--object-start-marker (o)
  (cond ((snippet--field-p o)
         (snippet--field-start o))
        ((snippet--mirror-p o)
         (snippet--mirror-start o))))

(defun snippet--object-end-marker (o)
  (cond ((snippet--field-p o)
         (snippet--field-end o))
        ((snippet--mirror-p o)
         (snippet--mirror-end o))))

(defun snippet--call-with-current-object (object fn)
  (let* ((start (snippet--object-start-marker object))
         (end (snippet--object-end-marker object))
         (start-itype (marker-insertion-type start))
         (end-itype (marker-insertion-type end)))
    (unwind-protect
        (progn
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
          (funcall fn))
      (set-marker-insertion-type start start-itype)
      (set-marker-insertion-type end end-itype))))

(defun snippet--insert-field (field text)
  (when text
    (snippet--with-current-object field
      (insert text))))

(defun snippet--insert-mirror (mirror)
  (snippet--with-current-object mirror
    (insert (funcall (snippet--mirror-transform mirror)))))

(defun snippet--field-text (field)
  (buffer-substring-no-properties (snippet--field-start field)
                                  (snippet--field-end field)))



;;; the define-snippet macro and its helpers
;;;


(defun snippet--form-field-p (form)
  (and (consp form) (eq (car form) 'field)))
(defun snippet--form-mirror-p (form)
  (and (consp form) (eq (car form) 'mirror)))
(defun snippet--form-make-field-sym (field-name &optional parent-field-sym)
  (make-symbol (format "field-%s%s" field-name
                       (if parent-field-sym
                           (format "-son-of-%s" parent-field-sym)
                         ""))))
(defun snippet--form-make-mirror-sym (mirror-name source-field-name &optional parent-field-sym)
  (make-symbol (format "mirror-%s-of-%s%s" mirror-name source-field-name
                       (if parent-field-sym
                           (format "-son-of-%s" parent-field-sym)
                         ""))))
(defun snippet--start-marker-name (sym)
  (make-symbol (format "%s-beg" sym)))

(defun snippet--end-marker-name (sym)
  (make-symbol (format "%s-end" sym)))





(defvar snippet--form-mirror-sym-idx nil)

(defun snippet--form-sym-tuples (forms &optional parent-field-sym)
  "Produce information for composing the snippet expansion function.

A tuple of 6 elements is created for each form in FORMS.

\(SYM FORM PARENT-FIELD-SYM ADJACENT-PREV-SYM PREV-FORM NEXT-FORM)

Forms representing fields with nested elements are recursively
iterated depth-first, resulting in a flattened list."
  (loop with snippet--form-mirror-sym-idx = (or snippet--form-mirror-sym-idx
                                                0)
        with adjacent-prev-sym

        for prev-form in (cons nil forms)
        for form in forms
        for next-form in (append (rest forms) (list nil))

        for (sym childrenp) = (cond ((snippet--form-field-p form)
                                     (list (snippet--form-make-field-sym (second form)
                                                                         parent-field-sym)
                                           (listp (third form))))
                                    ((snippet--form-mirror-p form)
                                     (incf snippet--form-mirror-sym-idx)
                                     (list (snippet--form-make-mirror-sym snippet--form-mirror-sym-idx
                                                                          (second form)
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

                     ((or (stringp form)
                          (symbolp form)
                          (eq (car form) 'lambda))
                      `((ignore ,form ,parent-field-sym))))
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
  (loop for (sym nil parent-sym adjacent-prev-sym prev next) in tuples
        unless (eq sym 'ignore)
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





(defun snippet--make-object-sym-tuples (tuples)
  (let* ((field-mirrors (make-hash-table))
         ;; we first collect `snippet--make-mirror' forms. When
         ;; collecting them, we populate the `field-mirrors' table...
         ;;
         (make-mirror-forms
          (loop for (sym form parent-sym) in tuples
                when (snippet--form-mirror-p form)
                collect (let ((source-sym nil))
                          (loop for (sym-b form-b) in tuples
                                when (and
                                      (snippet--form-field-p form-b)
                                      (eq (second form)
                                          (second form-b)))
                                do
                                (setq source-sym sym-b)
                                (puthash source-sym (cons sym (gethash source-sym field-mirrors)) field-mirrors))
                          (unless source-sym
                            (error "mirror definition %s mentions unknown field" form))
                          `(,sym (snippet--make-mirror ,source-sym
                                                       ,(snippet--transform-lambda (third form) source-sym)
                                                       ,parent-sym
                                                       ,(snippet--start-marker-name sym)
                                                       ,(snippet--end-marker-name sym))

                                 ))))
         ;; so that we can now create `snippet--make-field' forms with
         ;; complete lists of mirror symbols.
         ;;
         (make-field-forms
          (loop for (sym form parent-sym) in tuples
                when (snippet--form-field-p form)
                collect `(,sym (snippet--make-field ,(second form)
                                                    (list ,@(gethash sym field-mirrors))
                                                    ,parent-sym
                                                    ,(snippet--start-marker-name sym)
                                                    ,(snippet--end-marker-name sym))))))

    (append make-field-forms
            make-mirror-forms)))

(defun snippet--transform-lambda (transform-form source-sym)
  `(lambda ()
     (funcall
      #'(lambda (field-text)
          ,(or transform-form
               'field-text))
      (snippet--field-text ,source-sym))))


(defmacro define-snippet (name args &rest body)
  "Define NAME as a snippet.

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
  (let* ((sym-tuples (snippet--form-sym-tuples body))
         (marker-init-forms (snippet--make-marker-init-forms sym-tuples))
         (make-object-forms (snippet--make-object-sym-tuples sym-tuples)))
    `(let ((insert-snippet-fn
            #'(lambda ()
                (let* (,@(mapcar #'list (remove 'ignore (mapcar #'car sym-tuples)))
                       ,@marker-init-forms
                       (start (point))
                       overlay)

                  ,(if make-object-forms
                       `(setq ,@(loop for (sym form) in make-object-forms
                                      append (list sym form))))

                  ,@(loop
                     for (sym form)           in sym-tuples
                     collect (cond ((snippet--form-field-p form)
                                    `(snippet--insert-field ,sym ,(if (stringp (third form))
                                                                      (third form))))
                                   ((snippet--form-mirror-p form)
                                    `(snippet--insert-mirror ,sym))
                                   ((stringp form)
                                    `(insert ,form))
                                   ((functionp form)
                                    `(insert (funcall ,form)))))

                  (setq overlay (make-overlay start (point)))
                  overlay
                  ))))
       (defun ,name ()
         (funcall insert-snippet-fn)))))

(define-snippet test ()
  "some string" buffer-file-name)


(define-snippet printf ()
  "printf (\""
  (field 1 "%s")
  (mirror 1 (if (string-match "%" field-text) "\"," "\);"))
  (field 2)
  (mirror 1 (if (string-match "%" field-text) "\);" "")))


(define-snippet foo ()
  (field 1 "bla")
  "ble"
  (mirror 1)
  (field 2
         ((field 3 "fonix")
          "fotrix"
          (mirror 1 "qqcoisa")))
  "end")




(provide 'snippet)

;;; Local Variables:
;;; lexical-binding: t
;;; End:
;;; snippet.el ends here
