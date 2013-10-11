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


(cl-defstruct (snippet--field (:constructor snippet--make-field (name parent-field)))
  name
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)


(cl-defstruct (snippet--mirror (:constructor snippet--make-mirror (source transform)))
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


(defmacro* define-snippet (name (&key obarray) &rest body)
  (cl-flet ((field-p (form) (and (consp form) (eq (car form) 'field)))
            (mirror-p (form) (and (consp form) (eq (car form) 'mirror)))
            (make-field-sym (field-name) (make-symbol (format "field-%s"
                                                              field-name)))
            (make-mirror-sym (mirror-name field-name) (make-symbol (format "mirror-%s-of-%s"
                                                                           mirror-name
                                                                           field-name))))
    `(defun ,name ()
       (let* (,@(loop for form in (cl-remove-if-not #'field-p body)
                      for field-sym = (make-field-sym (second form))
                      collect `(,field-sym
                                (snippet--make-field ,(second form) ,(third form))))
              ,@(loop for form in (cl-remove-if-not #'mirror-p body)
                      for mirror-sym = (make-mirror-sym i (second form))
                      for source-field-sym = (make-field-sym (second form))
                      for i from 1
                      collect `(,mirror-sym
                                (snippet--make-mirror ,source-field-sym
                                                      #'(lambda ()
                                                          (funcall
                                                           #'(lambda (field-text)
                                                               ,(third form))
                                                           (snippet--field-text ,source-field-sym))))))
              (start (point))
              overlay)
         ,@(loop with mirror-idx = 1
                 for form in body
                 collect (cond ((field-p form)
                                `(snippet--insert-field ,(make-field-sym (second form))
                                                        ,(third form)))
                               ((mirror-p form)
                                (prog1 `(snippet--insert-mirror ,(make-mirror-sym mirror-idx
                                                                                  (second form)))
                                  (incf mirror-idx)))
                               (t
                                `(insert ,form))))
         (setq overlay (make-overlay start (point)))))))

(define-snippet printf ()
  "printf (\""
  (field 1 "%s")
  "\n"
  (mirror 1 (if (string-match "%" field-text) "," "\);"))
  (field 2)
  (mirror 1 (if (string-match "%" field-text) "\);" "")))




(provide 'snippet)

;;; Local Variables:
;;; lexical-binding: t
;;; End:
;;; snippet.el ends here
