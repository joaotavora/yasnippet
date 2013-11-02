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

(defun snippet--canonicalize-form (form)
  (pcase form
    ((or `&field `(&field))
     `(&field ,(cl-gensym "auto-") nil))
    (`(&field ,name)
     `(&field ,name nil))
    ((pred atom)
     `(&eval ,form))
    (`(&eval ,_)
     form)
    (`(&eval . ,_)
     (error "provide only one form after &eval in %S" form))
    (`(&mirror ,name)
     `(&mirror ,name (&transform field-string)))
    (`(&mirror ,_ (&transform ,_))
     form)
    (`(&field ,_ (,(or `&transform `&eval) ,_))
     form)
    (`(,(or `&mirror `&field) ,_ (,(or `&transform `&eval) ,_ . (,extra)))
     (error "expected one form after &eval or &transform in %S, you have %d"
            form (1+ (length extra))))
    (`(,(or `&mirror `&field) ,name ,_ . (,extra))
     (error "expected one form after '%S' in %S, you have %d"
            name
            form (1+ (length extra))))
    (`(&field ,name (&nested . ,more-forms))
     `(&field ,name (&nested . (mapcar #'snippet--canonicalize-form
                                       ,more-forms)))
     form)
    (`(&mirror ,name ,expr)
     `(&mirror ,name (&transform ,expr)))

    (`(&field ,name ,expr)
     `(&field ,name (&eval ,expr)))
    ((pred consp)
     `(&eval ,form))
    (t
     (error "invalid snippet form %s" form))))

(defun snippet--form-tuples (forms &optional parent-field-sym)
  "Produce information for composing the snippet insertion function.

A tuple of 6 elements is created for each form in FORMS.

\(SYM FORM PARENT-FIELD-SYM ADJACENT-PREV-SYM PREV-FORM NEXT-FORM)

Forms representing fields with nested elements are recursively
iterated depth-first, resulting in a flattened list."
  (cl-loop with forms = (mapcar #'snippet--canonicalize-form forms)
           with snippet--form-mirror-sym-idx = (or snippet--form-mirror-sym-idx
                                                   0)
           with sym
           with adjacent-prev-sym
           with has-children-p

           for (prev-form form next-form) on `(nil ,@forms)
           while form

           with collect-sym = #'(lambda () `(,sym ,form
                                                  ,parent-field-sym
                                                  ,adjacent-prev-sym
                                                  ,prev-form ,next-form))
           collect
           (pcase form
             (`(&field ,name ,expr)
              (setq sym (snippet--form-make-field-sym name
                                                      parent-field-sym)
                    has-children-p (and (listp expr)
                                        (eq '&nested (car expr))))

              (funcall collect-sym))

             (`(&mirror ,name ,_expr)
              (incf snippet--form-mirror-sym-idx)
              (setq sym (snippet--form-make-mirror-sym snippet--form-mirror-sym-idx
                                                       name
                                                       parent-field-sym))
              (funcall collect-sym))
             (`(&eval ,_expr)
              `,form))
           when has-children-p
           append (snippet--form-tuples (cdr (cl-third form)) sym)
           do (setq adjacent-prev-sym sym
                    sym nil
                    has-children-p nil)))

(defun snippet--marker-init-forms (tuples)
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
  (cl-loop for (sym nil parent-sym adjacent-prev-sym prev next)
           in (cl-remove '&eval tuples :key #'car)
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
                             (eq '&field (car form))))
                    tuples :key #'cadr))

(defun snippet--object-init-forms (tuples)
  (let* ((field-mirrors (make-hash-table))
         ;; we first collect `snippet--make-mirror' forms. When
         ;; collecting them, we populate the `field-mirrors' table...
         ;;
         (tuples (cl-remove '&eval tuples :key #'car))
         (make-mirror-forms
          (cl-loop for ((prev-sym)
                        (sym (type name (_ transform)) parent-sym)
                        (next-sym))
                   on `(nil ,@tuples)
                   when (and sym (eq '&mirror type))
                   collect (let ((source-sym nil))
                             (cl-loop for (sym-b (type-b name-b)) in tuples
                                      when (and
                                            (eq '&field type-b)
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
                                ,(snippet--start-marker-name sym)
                                ,(snippet--end-marker-name sym)
                                ,parent-sym
                                ,prev-sym
                                ,next-sym
                                ,source-sym
                                ,(snippet--transform-lambda transform))))))
         ;; so that we can now create `snippet--make-field' forms with
         ;; complete lists of mirror symbols.
         ;;
         (make-field-forms
          (cl-loop for ((prev-sym)
                        (sym (type name _value) parent-sym)
                        (next-sym))
                   on `(nil ,@tuples)
                   when (and sym (eq '&field type))
                   collect `((,sym (snippet--make-field))
                             (snippet--init-field
                              ,sym
                              ,(snippet--start-marker-name sym)
                              ,(snippet--end-marker-name sym)
                              ,parent-sym
                              ,prev-sym
                              ,next-sym
                              ,name
                              (list
                               ,@(reverse
                                  (gethash sym field-mirrors))))))))

    (append make-field-forms
            make-mirror-forms)))

(defun snippet--transform-lambda (transform-form)
  `(lambda (field-string field-empty-p)
     ,transform-form))

(defun snippet--eval-lambda (eval-form)
  `(lambda (region-string)
     ,eval-form))

(defun define--snippet-body (body)
  "Does the actual work for `define-snippet'"
  (let* ((tuples (snippet--form-tuples body))
         (marker-init-forms (snippet--marker-init-forms tuples))
         (init-object-forms (snippet--object-init-forms tuples))
         (first-field-sym (snippet--first-field-sym tuples))
         (region-text-sym (make-symbol "region-string")))
    `(let* (,@(mapcar #'car init-object-forms)
            ,@marker-init-forms
            (,region-text-sym (and (region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning)
                                    (region-end)))))

       ,@(mapcar #'second init-object-forms)

       ,@(cl-loop
          for (sym form) in tuples
          append (pcase form
                   (`(&field ,_ ,expr)
                    `((snippet--insert-object ,sym)
                      ,(when (eq `&eval (car expr))
                         `(snippet--with-current-object ,sym
                            (insert
                             (or (funcall ,(snippet--eval-lambda (cadr expr))
                                          ,region-text-sym)
                                 ""))))))
                   (`(&mirror . ,_)
                    `((snippet--insert-object ,sym)))
                   (t
                    `((insert (or (funcall ,(snippet--eval-lambda form)
                                           ,region-text-sym)
                                  " "))))))
       ,@(cl-loop
          for (sym form) in tuples
          append (pcase form
                   (`(&field . ,_)
                    `((mapc #'snippet--update-mirror
                            (snippet--field-mirrors ,sym))))))

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
                    (list ,@(remove '&eval (mapcar #'car tuples))))
       ,(if first-field-sym
            `(snippet--move-to-field ,first-field-sym))
       (add-hook 'post-command-hook 'snippet--post-command-hook t t))))


(cl-defmacro define-snippet (name () &rest snippet-forms)
  "Define NAME as a snippet-inserting function.

NAME's function definition is set to a function with no arguments
that inserts the snippet's components at point.

Each form in SNIPPET-FORMS, inserted at point in order, can be:

* A cons (&field FIELD-NAME FIELD-DEFAULT) definining a snippet
  field. A snippet field can be navigated to using
  `snippet-next-field' and `snippet-prev-field'. FIELD-NAME is
  optional and used for referring to the field in mirror
  transforms. FIELD-DEFAULT is also optional and used for
  producing a string that populates the field's default value at
  snippet-insertion time.

  FIELD-DEFAULT can thus be a string literal, a lisp form
  returning a string, or have the form (&nested SUB-FORM ...)
  where each SUB-FORM is evaluated recursively according to the
  rules of SNIPPET-FORMS.

  FIELD-DEFAULT can additionally also be (&transform
  FIELD-TRANSFORM) in which case the string value produced by
  FIELD-TRANSFORM is used for populating not only the field's
  default value, but also the field's value after each command
  while the snippet is alive.

* A cons (&mirror FIELD-NAME MIRROR-TRANSFORM) defining a mirror
  of the field named FIELD-NAME. MIRROR-TRANSFORM is optional and
  is called after each command while the snippet is alive to
  produce a string that becomes the mirror text.

* A string literal or a lisp form CONSTANT evaluated at
  snippet-insertion time and producing a string that is a part of
  the snippet but constant while the snippet is alive.

* A form (&exit EXIT-DEFAULT), defining the point within the
  snippet where point should be placed when the snippet is
  exited. EXIT-DEFAULT is optional and is evaluated at
  snippet-insertion time to produce a string that remains a
  constant part of the snippet while it is alive, but is
  automatically selected when the snippet is exited.

The forms CONSTANT, FIELD-DEFAULT, MIRROR-TRANSFORM,
FIELD-TRANSFORM and EXIT-DEFAULT are evaluated with the variable
`region-string' set to the text of the buffer selected at
snippet-insertion time. If no region was selected the value of
this variable is the empty string..

The forms MIRROR-TRANSFORM and FIELD-TRANSFORM are evaluated with
the variable `field-string' set to the text contained in the
corresponding field. If the field is empty, this variable is the
empty string and the additional variable `field-empty-p' is t. If
these forms return nil, they are considered to have returned the
empty string.

If the form CONSTANT returns nil or the empty string, it is
considered to have returned a single whitespace.

ARGS is an even-numbered property list of (KEY VAL) pairs. Its
meaning is not decided yet"
  (declare (debug (&define name sexp &rest snippet-form)))
  `(defun ,name ()
     ,(define--snippet-body snippet-forms)))

(def-edebug-spec snippet-form
  (&or
   ("&mirror" sexp def-form)
   ("&field" sexp &or ("&nested" &rest snippet-form) def-form)
   def-form))

(defun make-snippet (forms)
  "Same as `define-snippet', but return an anonymous function."
  `(lambda () ,(define--snippet-body forms)))


;;; Snippet mechanics
;;;

(cl-defstruct snippet--object
  start end parent-field next prev (buffer (current-buffer)))

(defun snippet--init-object (object start end parent-field prev next)
  (setf (snippet--object-start object) start
        (snippet--object-end object) end
        (snippet--object-parent-field object) parent-field
        (snippet--object-next object) next
        (snippet--object-prev object) prev))

(cl-defstruct (snippet--field (:constructor snippet--make-field ())
                              (:include snippet--object)
                              (:print-function snippet--describe-field))
  name
  (mirrors '())
  (modified-p nil))

(defun snippet--describe-field (field)
  (with-current-buffer (snippet--object-buffer field)
    (format "field %s from %s to %s covering \"%s\""
            (snippet--field-name field)
            (marker-position (snippet--object-start field))
            (marker-position (snippet--object-end field))
            (buffer-substring-no-properties
             (snippet--object-start field)
             (snippet--object-end field)))))

(defun snippet--init-field (object start end parent-field prev next
                                   name mirrors)
  (snippet--init-object object start end parent-field prev next)
  (setf (snippet--field-name object) name
        (snippet--field-mirrors object) mirrors))

(cl-defstruct (snippet--mirror (:constructor snippet--make-mirror ())
                               (:include snippet--object)
                               (:print-function snippet--describe-mirror))
  source
  (transform nil))

(defun snippet--init-mirror (object start end parent-field prev next
                                    source transform)
  (snippet--init-object object start end parent-field prev next)
  (setf (snippet--mirror-source object) source
        (snippet--mirror-transform object)  transform))

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
    (define-key map (kbd "<backtab>")   'snippet-prev-field)
    map)
  "The active keymap while a live snippet is being navigated.")

(defvar snippet--field-overlay nil)

(defun snippet--object-next-field (object)
  (loop for next = (snippet--object-next object)
        then (snippet--object-next next)
        while (and next)
        when (and (snippet--field-p next)
                  (not (snippet--field-skip-p next)))
        return next))

(defun snippet--object-prev-field (object)
  (loop for prev = (snippet--object-prev object)
        then (snippet--object-prev prev)
        while prev
        when (and (snippet--field-p prev)
                  (not (snippet--field-skip-p prev)))
        return prev))

(defun snippet--field-skip-p (field)
  (let ((parent (snippet--field-parent-field field)))
    (and parent
         (snippet--object-empty-p field)
         (snippet--field-modified-p parent))))

(defun snippet-next-field (&optional prev)
  (interactive)
  (let* ((field (overlay-get snippet--field-overlay 'snippet--field))
         (target (if prev
                     (snippet--object-prev-field field)
                   (snippet--object-next-field field))))
    (if target
        (snippet--move-to-field target)
      (unless prev
        (goto-char (snippet--object-end field)))
      (snippet-exit-snippet))))

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
  (point-marker))

(defun snippet--object-empty-p (object)
  (= (snippet--object-start object)
     (snippet--object-end object)))

(defun snippet--objects-adjacent-p (prev next)
  (eq (snippet--object-end prev)
      (snippet--object-start next)))

(defun snippet--open-object (object &optional close-instead)
  (let ((stay (cons (snippet--object-start object)
                    (cl-loop for o = object then prev
                             for prev = (snippet--object-prev o)
                             while (and prev
                                        (snippet--objects-adjacent-p prev o)
                                        (snippet--object-empty-p prev))
                             collect (snippet--object-start prev))))
        (push (cons (snippet--object-end object)
                    (cl-loop for o = object then next
                             for next = (snippet--object-next o)
                             while (and next
                                        (snippet--objects-adjacent-p o next)
                                        (snippet--object-empty-p next))
                             collect (snippet--object-end next)))))
    (when close-instead
      (if (snippet--object-empty-p object)
          (setq stay (append stay push)
                push nil)
        (cl-rotatef stay push)))
    (mapc #'(lambda (m) (set-marker-insertion-type m nil)) stay)
    (mapc #'(lambda (m) (set-marker-insertion-type m t)) push)))

(defun snippet--call-with-current-object (object fn)
  (unwind-protect
      (progn
        (snippet--open-object object)
        (funcall fn))
    (snippet--open-object object 'close)))

(defmacro snippet--with-current-object (object &rest body)
  (declare (indent defun) (debug t))
  `(snippet--call-with-current-object ,object #'(lambda () ,@body)))

(defun snippet--insert-object (object)
  (set-marker (snippet--object-start object) (point))
  (set-marker (snippet--object-end object) (point)))

(defun snippet--update-mirror (mirror)
  (snippet--with-current-object mirror
    (delete-region (snippet--object-start mirror)
                   (snippet--object-end mirror))
    (save-excursion
      (goto-char (snippet--object-start mirror))
      (let ((field-string (snippet--field-string (snippet--mirror-source mirror))))
        (insert (or (funcall (snippet--mirror-transform mirror)
                             field-string
                             (string= "" field-string))
                    ""))))))

(defun snippet--move-to-field (field)
  (goto-char (snippet--object-start field))
  (move-overlay snippet--field-overlay
                (point)
                (snippet--object-end field))
  (overlay-put snippet--field-overlay 'snippet--field field))

(defun snippet--update-field-mirrors (field)
  (mapc #'snippet--update-mirror (snippet--field-mirrors field))
  (when (snippet--object-parent-field field)
    (snippet--update-field-mirrors (snippet--object-parent-field field))))

(defun snippet--field-overlay-changed (overlay after? beg end
                                               &optional pre-change-len)
  ;; there's a slight (apparently innocuous) bug here: if the overlay has
  ;; zero-length, both `insert-in-front' and `insert-behind' modification hooks
  ;; are called
  ;;
  (let* ((field (overlay-get overlay 'snippet--field))
         (inhibit-modification-hooks t))
    (cond (after?
           ;; field clearing: if we're doing an insertion and the field hasn't
           ;; been modified yet, we're going to delete previous contents and
           ;; leave just the newly inserted text.
           ;;
           (when (and (not (snippet--field-modified-p field))
                      (= beg (snippet--field-start field))
                      (zerop pre-change-len))
             ;; At first glance, we could just delete the region between `end'
             ;; and the `field's end, but that wouldn't empty any child fields
             ;; that `field' might have, since that child's markers, albeit
             ;; closed, may will have legitimately moved to accomodate the
             ;; insertion. So we save the text, delete the entire field contents
             ;; and insert it back in place. The child's markers will move
             ;; together.
             ;;
             (let ((saved (buffer-substring beg end)))
               (delete-region (snippet--object-start field)
                              (snippet--object-end field))
               (insert saved)))
           (setf (snippet--field-modified-p field) t)
           (snippet--update-field-mirrors field)
           (move-overlay overlay
                         (snippet--object-start field)
                         (snippet--object-end field)))
          (t
           (snippet--open-object field)))))

(defun snippet--field-string (field)
  (let ((start (snippet--object-start field))
        (end (snippet--object-end field)))
    (buffer-substring-no-properties start end)))

(defvar snippet--debug nil)
;; (setq snippet--debug t)
;; (setq snippet--debug nil)

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
