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


(cl-defstruct (snippet--field (:constructor snippet--make-field (name mirrors parent-field)))
  name
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)


(cl-defstruct (snippet--mirror (:constructor snippet--make-mirror (source transform parent-field)))
  source
  start end
  (transform nil)
  parent-field
  next
  depth)

(defun snippet--insert-field (field text)
  (setf (snippet--field-start field) (point))
  (when text (insert text))
  (setf (snippet--field-end field) (point)))

(defun snippet--insert-mirror (mirror)
  (setf (snippet--mirror-start mirror) (point))
  (insert (funcall (snippet--mirror-transform mirror)))
  (setf (snippet--mirror-end mirror) (point)))

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
(defun snippet--form-make-field-form (field-name parent-field-sym mirrors)
  `(snippet--make-field ,field-name ,parent-field-sym ,mirrors))
(defun snippet--form-make-mirror-form (source-field-sym transform)
  `(snippet--make-mirror ,source-field-sym ,transform))


(defvar snippet--form-mirror-sym-idx nil)
(defun snippet--form-sym-tuples (forms &optional parent-field-sym)
  (loop with snippet--form-mirror-sym-idx = (or snippet--form-mirror-sym-idx
                                                0)
        for form in forms
        append (cond ((snippet--form-field-p form)
                      ;;
                      ;;
                      (let ((field-sym (snippet--form-make-field-sym (second form) parent-field-sym)))
                        `((,field-sym ,form ,parent-field-sym)
                          ,@(when (listp (third form))
                              (snippet--form-sym-tuples (third form) field-sym)))))

                     ((snippet--form-mirror-p form)
                      ;;
                      ;;
                      (incf snippet--form-mirror-sym-idx)
                      (let ((mirror-sym (snippet--form-make-mirror-sym snippet--form-mirror-sym-idx
                                                                       (second form)
                                                                       parent-field-sym)))
                        `((,mirror-sym ,form ,parent-field-sym))))

                     (t
                      ;; it's a literal string, append a dummy tuple
                      `((string ,form))))))

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
                                                            ,parent-sym)))))
         ;; so that we can now create `snippet--make-field' forms with
         ;; complete lists of mirror symbols.
         ;;
         (make-field-forms
          (loop for (sym form parent-sym) in tuples
                when (snippet--form-field-p form)
                collect `(,sym (snippet--make-field ,(second form) (list ,@(gethash sym field-mirrors)) ,parent-sym)))))

    (append make-field-forms
            make-mirror-forms)))

(defun snippet--transform-lambda (transform-form source-sym)
  `(lambda ()
     (funcall
      #'(lambda (field-text)
          ,(or transform-form
               'field-text))
      (snippet--field-text ,source-sym))))


(defmacro* define-snippet (name (&key obarray) &rest body)
  (let* ((sym-tuples (snippet--form-sym-tuples body))
         (make-object-forms (snippet--make-object-forms sym-tuples)))
    `(defun ,name ()
       (let* (,@(remove 'string (mapcar #'car sym-tuples))
              (start (point))
              overlay)
         ,@make-object-forms


         ,@(loop for (sym form) in sym-tuples
                 collect (cond ((snippet--form-field-p form)
                                `(snippet--insert-field ,sym ,(third form)))
                               ((snippet--form-mirror-p form)
                                `(snippet--insert-mirror ,sym))
                               (t
                                `(insert ,form))))

         (setq overlay (make-overlay start (point)))))))

(define-snippet printf ()
  "printf (\""
  (field 1 "%s")
  (mirror 1 (if (string-match "%" field-text) "," "\);"))
  (field 2)
  (mirror 1 (if (string-match "%" field-text) "\);" "")))

(printf)

(ert-deftest snippet--test-form-sym-tuples ()
  (let* ((test-tuples (snippet--form-sym-tuples '((field 1 "bla")
                                                  "ble"
                                                  (mirror 1)
                                                  (field 2
                                                         ((field 3 "fonix")
                                                          "fotrix"
                                                          (mirror 1 "qqcoisa")))
                                                  "end")))
         (symbols-declared (mapcar #'(lambda (sym) (intern (symbol-name sym)))
                                   (remove 'string (mapcar #'car test-tuples)))))
    (should
     (equal '(field-1 mirror-1-of-1 field-2 field-3-son-of-field-2 mirror-2-of-1-son-of-field-2)
            symbols-declared))))


(provide 'snippet)

;;; Local Variables:
;;; lexical-binding: t
;;; End:
;;; snippet.el ends here
