;;; snippet.el --- yasnippet's engine distilled  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Free Software Foundation

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

;; The yasnippet snippet insertion and navigation engine, though very powerful,
;; is bloated and its implementation much contrived.
;;
;; snippet.el's main goal is to provide yasnippet and other snippet-managing
;; frontends with the bare minimum funcionality to define, insert, navigate and
;; undo snippets.
;;
;; Snippets are defined via the `define-dynamic-snippet' or
;; `define-static-snippet' entrypoints. The snippet definition syntax is quite
;; different (TODO: how so?). Static snippets have better syntax checks at
;; compile-time, but complex snippets may be easier to write as dynamic
;; snippets. Both are as powerful as yasnippet's, in turn inspired by
;; textmate's). There are also `with-dynamic-snippet' and `with-static-snippet'
;; macros to use in your own defuns.
;;
;; The snippet definitions defined with both macros `define-*-snippet' macros
;; can be `edebug'ed and stepped through on snippet insertion and
;; navigation. (TODO: for `define-dynamic-snippet', this works for forms in
;; snippet fields, but fails for mirrors for some reason.)
;;
;; Once inserted into a buffer, snippets are navigated using
;; `snippet-next-field' and `snippet-prev-field', bound to TAB and S-TAB by
;; default.
;;
;; Funcionality such as snippet management, mode management, tab-trigerring or
;; any other fanciness are *not* offered.  `define-snippet's PROPERTIES
;; mechanism *might* become extensible to provide frontends such as yasnippet
;; the capability to conveniently implement said fanciness.
;;
;; TODO: auto-indentation of the inserted snippet
;; 
;; TODO: there should be somewhere a PROPERTIES argument, unimplemented at time
;; of writing, in the `define-snippet' macro which should probably understand a
;; set of properties controlling snippet's indentation, custom keymapping and
;; other per-snippet characteristics affecting snippet.el's core functionality.
;; 
;; TODO: undo, specifically snippet revival
;;
;; TODO: more documentation
;;
;; TODO: "protection overlays" or some robust mechanism preventing the use from
;;       inadvertenly destroying the snippet's structure and leaving its
;;       incoherent carcass behind.  Alternatively, detect this and exit the
;;       snippet beforehand.
;;
;; TODO: primary field transformations: the (&transform ...) option to &field
;;       constructs.
;;
;; TODO: (&exit ...) constructs with a default string should leave it marked and
;;       highlighted in the buffer on snippet-exit.  Yasnippet tried, but does
;;       not support this correctly.
;;
;; TODO: stacked insertion, aka snippet-in-snippet: the ability to expand a
;;       second snippet while navigating a snippet's field, and returning to the
;;       original field after exiting the second snippet.
;;
;; TODO: sub-snippets: the ability to insert navigable sub-snippets into a
;;       snippet definition by chaining them to the adjoining objects.  Yasnippet
;;       does not provide this.  Might become a (&snippet ...) construct to
;;       `define-snippet'.
;;
;; TODO: dynamic fields: the ability dinamically add snippet fields and mirrors
;;       to the snippet currently being navigated.  Yasnippet does not provide
;;       this and seems hard to do.  Might become obsolete if stacked insertion
;;       and primary field transformations are implemented nicely.
;;

;;; Code:

(require 'cl-lib)
(require 'eieio)


;;; the `define-static-snippet' macro and its helpers
;;;
(defvar snippet--sym-obarray (make-vector 100 nil))

(defun snippet--make-field-sym (field-name)
  (intern (format "field-%s" field-name) snippet--sym-obarray))

(defun snippet--make-mirror-sym (mirror-name source-field-name)
  (intern (format "mirror-%s-of-%s" mirror-name
                  source-field-name)
          snippet--sym-obarray))

(defun snippet--make-exit-sym ()
  (intern "exit" snippet--sym-obarray))

(defun snippet--make-transform-lambda (transform-form)
  `(lambda (field-string field-empty-p)
     ,transform-form))

(defun snippet--make-lambda (eval-form)
  `#'(lambda (region-string)
       ,eval-form))

(defun snippet--canonicalize-form (form)
  (pcase form
    ((or `&field `(&field))
     `(&field ,(cl-gensym "auto") nil))
    (`(&field ,name)
     `(&field ,name nil))
    (`(&eval ,_)
     form)
    (`(&eval . ,_)
     (error "Provide only one form after &eval in %S" form))
    (`(&mirror ,name)
     `(&mirror ,name (&transform field-string)))
    (`(&mirror ,_ (&transform ,_))
     form)
    (`(&field ,_ (,(or `&transform `&eval) ,_))
     form)
    (`(,(or `&mirror `&field) ,_ (,(or `&transform `&eval) ,_ . (,extra)))
     (error "Expected one form after &eval or &transform in %S, you have %d"
            form (1+ (length extra))))
    (`(,(or `&mirror `&field) ,name ,_ . (,extra))
     (error "Expected one form after '%S' in %S, you have %d"
            name
            form (1+ (length extra))))
    (`(&field ,name (&nested . ,more-forms))
     `(&field ,name (&nested ,@(mapcar #'snippet--canonicalize-form
                                       more-forms))))
    (`(&mirror ,name ,expr)
     `(&mirror ,name (&transform ,expr)))

    (`(&field ,name ,expr)
     `(&field ,name (&eval ,expr)))

    (`(&exit ,expr)
     `(&exit (&eval ,expr)))
    ((or `&exit `(&exit))
     `(&exit (&eval nil)))
    ((pred atom)
     `(&eval ,form))
    ((pred consp)
     `(&eval ,form))
    (t
     (error "Invalid snippet form %s" form))))

(defun snippet--unfold-forms (forms &optional parent-sym)
  (cl-loop for form in forms
           collect (append form
                           `((&parent ,parent-sym)))
           append (pcase form
                    (`(&field ,name (&nested . ,subforms))
                     (snippet--unfold-forms subforms
                                            (snippet--make-field-sym name))))))

(defmacro with-static-snippet (&rest forms)
  "Define and insert a snippet from FORMS.
As `define-static-snippet' but doesn't define a function."
  (let ((unfolded (snippet--unfold-forms
                   (mapcar #'snippet--canonicalize-form forms)))
        all-objects exit-object)
    `(let* (,@(loop for form in unfolded
                    append (pcase form
                             (`(&field ,name ,_expr (&parent ,parent))
                              `((,(snippet--make-field-sym name)
                                 (make-instance 'snippet--field
                                                :parent ,parent
                                                :name ',name))))))
            (region-string (and (region-active-p)
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end)))))
       (let* (,@(loop
                 for form in unfolded
                 with mirror-idx = 0
                 with sym
                 with prev-sym
                 append
                 (pcase form
                   (`(&field ,name ,expr (&parent ,_parent))
                    (setq sym (snippet--make-field-sym name))
                    `((,sym (snippet--insert-field
                             ,sym
                             ,prev-sym
                             ,(pcase expr
                                (`(&eval ,form)
                                 `(funcall ,(snippet--make-lambda form)
                                           region-string)))))))
                   (`(&mirror ,name (&transform ,transform) (&parent ,parent))
                    (setq sym (snippet--make-mirror-sym
                               (cl-incf mirror-idx) name))
                    `((,sym (snippet--make-and-insert-mirror
                             ,parent
                             ,prev-sym
                             ,(snippet--make-transform-lambda transform) 
                             ,(snippet--make-field-sym name)))))
                   (`(&exit (&eval ,form) (&parent ,parent))
                    (when exit-object
                      (error "Too many &exit forms given"))
                    (setq sym (snippet--make-exit-sym)
                          exit-object sym)
                    `((,sym (snippet--make-and-insert-exit
                             ,parent
                             ,prev-sym
                             ,(and form
                                   `(funcall ,(snippet--make-lambda form)
                                             region-string))))))
                   (`(&eval ,form (&parent ,parent))
                    `((,(cl-gensym "constant-")
                       (snippet--insert-constant
                        ,parent
                        (funcall ,(snippet--make-lambda form)
                                 region-string))))))
                 when sym do
                 (push sym all-objects)
                 (setq prev-sym sym)
                 (setq sym nil)))
         (snippet--activate-snippet (list ,@all-objects))))))

(def-edebug-spec snippet-form
  (&or
   ("&mirror" sexp def-form)
   ("&field" sexp &or ("&nested" &rest snippet-form) def-form)
   def-form))

(defmacro define-static-snippet (name _properties &optional docstring
                                      &rest snippet-forms)
  "Make a snippet-inserting function from FORMS.

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
  of the field named FIELD-NAME. MIRROR-TRANSFORM is an optional
  form, called after each command while the snippet is alive to
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

PROPERTIES is an even-numbered property list of (KEY VAL)
pairs. Its meaning is not decided yet"
  (declare (debug (&define name sexp &rest snippet-forms))
           (indent defun))
  (unless (stringp docstring)
    (push docstring snippet-forms)
    (setq docstring nil))
  `(defun ,name () ,docstring
          (with-static-snippet ,@snippet-forms)))


;;; The `define-dynamic-snippet' macro
;;;
(defmacro with-dynamic-snippet (&rest body)
  `(let (;; (start (point-marker))
         (snippet--fields (make-hash-table))
         (snippet--mirrors (make-hash-table))
         (snippet--current-field)
         (snippet--prev-object)
         (snippet--all-objects))
     (cl-macrolet ((&field (&optional (field-name nil field-name-provided-p) &body field-forms)
                     (unless field-name-provided-p
                       (setf field-name (make-symbol "_ignored")))
                     `(let ((fn (lambda () ,@field-forms))
                            (field
                             (setf (gethash ',field-name snippet--fields)
                                   (make-instance 'snippet--field
                                                  :name ',field-name
                                                  :parent snippet--current-field))))
                        (snippet--inserting-object
                          field snippet--prev-object
                          (setf snippet--prev-object field)
                          (let* ((snippet--current-field field)
                                 (retval (funcall fn)))
                            (when (stringp retval) (insert retval))))
                        (push field snippet--all-objects)))
                   (&mirror (field-name &optional (mirror-args nil mirror-args-provided-p) &body mirror-forms)
                     (cond ((not mirror-args-provided-p)
                            (setq mirror-args `(,(intern "field-string") ,(make-symbol "_ignored")))
                            (setq mirror-forms `((insert ,(intern "field-string")))))
                           ((> (length mirror-args) 2)
                            (error "At most two args in mirror transforms"))
                           (t
                            (nconc mirror-args
                                   (cl-loop for i from (length mirror-args)
                                            below 2
                                            collect (make-symbol "_ignored")))))
                     `(let* ((fn (lambda ,mirror-args ,@mirror-forms))
                             (mirror (make-instance 'snippet--mirror
                                                    :parent snippet--current-field
                                                    :transform fn)))
                        (push mirror (gethash ',field-name snippet--mirrors))
                        (snippet--inserting-object mirror snippet--prev-object)
                        (setf snippet--prev-object mirror)
                        (push mirror snippet--all-objects)))
                   (&exit ()
                     `(let ((exit (make-instance 'snippet--exit
                                                 :parent snippet--current-field)))
                        (snippet--inserting-object exit snippet--prev-object)
                        (setf snippet--prev-object exit)
                        (push exit snippet--all-objects))))
       ,@body
       (maphash (lambda (field-name mirrors)
                  (let ((field (gethash field-name snippet--fields)))
                    (unless field
                      (error "Snippet mirror references field \"%s\" which does not exist!"
                             field-name))
                    (mapc (lambda (mirror)
                            (push mirror (snippet--field-mirrors field))
                            (setf (snippet--mirror-source mirror) field))
                          mirrors)))
                snippet--mirrors)
       (snippet--activate-snippet snippet--all-objects))))


(defmacro define-dynamic-snippet (name args &optional docstring &rest body)
  (declare (debug (&define name sexp def-body))
           (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  `(defun ,name ,args ,docstring
          (with-dynamic-snippet ,@body)))

(def-edebug-spec &mirror (sexp &optional sexp &rest form))
(def-edebug-spec &field (sexp &rest form))

(put '&field 'lisp-indent-function 'defun)
(put '&mirror 'lisp-indent-function 'defun)
(put '&exit 'lisp-indent-function 'defun)


;;; Snippet mechanics
;;;

(defclass snippet--object ()
  ((start :initarg :start :accessor snippet--object-start)
   (end :initarg :end :accessor snippet--object-end)
   (parent :initarg :parent :reader snippet--object-parent)
   (prev :initarg :prev :accessor snippet--object-prev)
   (next :initarg :next :accessor snippet--object-next)
   (buffer :initform (current-buffer) :reader snippet--object-buffer)))

(defclass snippet--field (snippet--object)
  ((name :initarg :name :accessor snippet--field-name)
   (modified-p :initform nil :accessor snippet--field-modified-p)
   (mirrors :initform (list) :accessor snippet--field-mirrors))
  :documentation "coiso")

(defclass snippet--mirror (snippet--object)
  ((source :initarg :source :accessor snippet--mirror-source)
   (transform :initarg :transform :accessor snippet--mirror-transform))
  :documentation "coiso")

(defclass snippet--exit (snippet--object) ())

(defun snippet--call-with-inserting-object (object prev fn)
  (when prev
    (setf (snippet--object-prev object) prev)
    (cl-assert (null (snippet--object-next prev)) nil
               "previous object already has another sucessor")
    (setf (snippet--object-next prev) object))
  (setf (snippet--object-start object)
        (let ((parent (snippet--object-parent object)))
          (cond ((and parent
                      (= (point) (snippet--object-start parent)))
                 (snippet--object-start parent))
                ((and prev
                      (snippet--object-parent prev)
                      (= (point) (snippet--object-end
                                  (snippet--object-parent prev))))
                 (snippet--object-end
                  (snippet--object-parent prev)))
                ((and prev
                      (snippet--object-end prev)
                      (= (point) (snippet--object-end prev)))
                 (snippet--object-end prev))
                (t
                 (point-marker)))))
  (funcall fn)
  ;; Don't set the object's end if its already set and matches point. i.e. when
  ;; running its function some nested field might have set it already and 
  (unless (and (snippet--object-end object)
               (= (snippet--object-end object) (point)))
    (setf (snippet--object-end object)
          (point-marker)))
  (when (snippet--object-parent object)
    (setf (snippet--object-end
           (snippet--object-parent object))
          (snippet--object-end object)))
  (snippet--open-object object 'close)
  object)

(defmacro snippet--inserting-object (object prev &rest body)
  (declare (indent defun) (debug (sexp sexp &rest form)))
  `(snippet--call-with-inserting-object ,object ,prev #'(lambda () ,@body)))

(defun snippet--insert-field (field prev default)
  (snippet--inserting-object field prev
    (when default
      (insert default))))

(defun snippet--make-and-insert-mirror (parent prev transform &optional source)
  (let ((mirror (make-instance 'snippet--mirror
                               :parent parent
                               :prev prev
                               :source source
                               :transform transform)))
    (when source
      (pushnew mirror (snippet--field-mirrors source)))
    (snippet--inserting-object mirror prev)))

(defun snippet--make-and-insert-exit (parent prev constant)
  (let ((exit (make-instance 'snippet--exit :parent parent :prev prev)))
   (snippet--inserting-object exit prev
     (when constant
       (insert constant)))))

(defun snippet--insert-constant (parent constant)
  (when constant
    (insert constant))
  (when parent
    (setf (snippet--object-end parent) (point-marker))))

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

(defun snippet--update-mirror (mirror)
  (snippet--with-current-object mirror
    (delete-region (snippet--object-start mirror)
                   (snippet--object-end mirror))
    (save-excursion
      (goto-char (snippet--object-start mirror))
      (let* ((field-string (snippet--field-string (snippet--mirror-source mirror)))
             (retval (funcall (snippet--mirror-transform mirror)
                              field-string
                              (string= "" field-string))))
        (when (stringp retval)
          (insert retval))))))

(defvar snippet--field-overlay nil)

(defun snippet--move-to-field (field)
  (goto-char (snippet--object-start field))
  (move-overlay snippet--field-overlay
                (point)
                (snippet--object-end field))
  (overlay-put snippet--field-overlay 'snippet--field field))

(defun snippet--update-field-mirrors (field)
  (mapc #'snippet--update-mirror (snippet--field-mirrors field))
  (when (snippet--object-parent field)
    (snippet--update-field-mirrors (snippet--object-parent field))))

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
                      (= beg (snippet--object-start field))
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


;;; Interactive
;;;
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

(defun snippet--field-skip-p (field)
  (let ((parent (snippet--object-parent field)))
    (and parent
         (snippet--object-empty-p field)
         (snippet--field-modified-p parent))))

(defun snippet-next-field (&optional prev)
  "Move to the start of next field in the current snippet.

Skips over nested fields if their parent has been modified.

PREV means move to the previous field."
  (interactive)
  (let* ((field (overlay-get snippet--field-overlay 'snippet--field))
         (sorted (overlay-get snippet--field-overlay 'snippet--fields))
         (sorted (if prev (reverse sorted) sorted))
         (target (if field
                     (cadr (cl-remove-if #'snippet--field-skip-p
                                         (memq field sorted)))
                   (first sorted))))
    (if target
        (snippet--move-to-field target)
      (let ((exit (overlay-get snippet--field-overlay
                               'snippet--exit)))
        (goto-char (if (markerp exit)
                       exit
                       (snippet--object-start exit))))
      (snippet-exit-snippet "moved to exit"))))

(defun snippet-prev-field ()
  "Move the the start of the previous field in the current snippet.

Skips over nested fields if their parent has been modified."
  (interactive)
  (snippet-next-field t))

(defun snippet-exit-snippet (&optional reason)
  (overlay-put snippet--field-overlay 'snippet--exit-reason reason))


;;; Main
;;;
(defvar snippet--debug nil)
;; (setq snippet--debug t)
;; (setq snippet--debug nil)

(defun snippet--activate-snippet (objects)
  (let ((mirrors (cl-sort
                  (cl-copy-list
                   (cl-remove-if-not #'snippet--mirror-p objects))
                  #'(lambda (p1 p2)
                      (cond ((not p2) t)
                            ((not p1) nil)))
                  :key #'snippet--object-parent))
        (fields (cl-sort
                 (cl-copy-list (cl-remove-if-not #'snippet--field-p objects))
                 #'(lambda (n1 n2)
                     (cond ((not (integerp n2)) t)
                           ((not (integerp n1)) nil)
                           (t (< n1 n2))))
                 :key #'snippet--field-name))
        (exit (or
               (cl-find-if #'snippet--exit-p objects)
               (let ((marker (point-marker)))
                 (prog1 marker
                   (set-marker-insertion-type marker t))))))
    (mapc #'snippet--update-mirror mirrors)
    (setq snippet--field-overlay
          (let ((overlay (make-overlay (point) (point) nil nil t)))
            (overlay-put overlay 'snippet--objects objects)
            (overlay-put overlay 'snippet--fields  fields)
            (overlay-put overlay 'snippet--exit    exit)
            (overlay-put overlay 'snippet--exit-reason nil)
            (overlay-put overlay 'face '           snippet-field-face)
            (overlay-put overlay
                         'modification-hooks
                         '(snippet--field-overlay-changed))
            (overlay-put overlay
                         'insert-in-front-hooks
                         '(snippet--field-overlay-changed))
            (overlay-put overlay
                         'insert-behind-hooks
                         '(snippet--field-overlay-changed))
            (overlay-put overlay
                         'keymap
                         snippet-field-keymap)
            overlay))
    (snippet-next-field)
    (add-hook 'post-command-hook 'snippet--post-command-hook 'append 'local)))

(defun snippet--post-command-hook ()
  ;; TODO: exiting the snippet might someday run user-provided code, hence the
  ;; apparent overengineeredness
  ;; 
  (let ((remove-self "unknown reason")
        (exit-reason nil))
    (cond
     ((and snippet--field-overlay
           (not (overlay-buffer snippet--field-overlay)))
      ;; Something deleted the overlay 
      (setq remove-self t
            exit-reason "overlay destroyed"))
     (snippet--field-overlay
      (setq exit-reason
            (or (overlay-get snippet--field-overlay
                             'snippet--exit-reason)
                (and (or (< (point)
                            (overlay-start snippet--field-overlay))
                         (> (point)
                            (overlay-end snippet--field-overlay)))
                     "point left snippet")))
      (setq remove-self (and exit-reason t)))
     (t
      (setq remove-self "shouldn't be there"
            exit-reason "no overlay")))
    (when remove-self
      (unless (eq remove-self t)
        (display-warning
         'snippet
         (format "Forced remove snippet--post-command-hook (%s)" remove-self))
        ;; in this case, even try to remove if globally
        ;;
        (remove-hook 'post-command-hook 'snippet--post-command-hook))
      (remove-hook 'post-command-hook 'snippet--post-command-hook 'local))
    (if (and snippet--debug snippet--field-overlay)
        (snippet--debug-snippet snippet--field-overlay))
    (when exit-reason
      (when snippet--field-overlay
        (delete-overlay snippet--field-overlay)
        (setq snippet--field-overlay nil))
      (message "snippet exited (%s)" exit-reason))))


;;; Debug helpers
;;;
(cl-defmethod snippet--describe-object ((object snippet--object) &key _short)
  (with-current-buffer (snippet--object-buffer object)
    (format
     "covering \"%s\"\n  from %s\n  to %s\n  next: %s\n  prev: %s\n  parent: %s"
     (propertize (buffer-substring
                  (snippet--object-start object)
                  (snippet--object-end object))
                 'face
                 'highlight)
     (snippet--object-start object)
     (snippet--object-end object)
     (if (snippet--object-next object)
         (snippet--describe-object (snippet--object-next object) :short t))
     (if (snippet--object-prev object)
         (snippet--describe-object (snippet--object-prev object) :short t))
     (if (snippet--object-parent object)
         (snippet--describe-object (snippet--object-parent object) :short t)))))

(cl-defmethod snippet--describe-object ((field snippet--field) &key short)
  (let ((active-field
         (overlay-get snippet--field-overlay 'snippet--field)))
    (with-current-buffer (snippet--object-buffer field)
      (format "%sfield %s %s"
              (if (and (not short) (eq field active-field))
                  (propertize "*active* " 'face 'snippet-field-face)
                "")
              (snippet--field-name field)
              (if short "" (cl-call-next-method))))))

(cl-defmethod snippet--describe-object ((mirror snippet--mirror) &key short)
  (with-current-buffer (snippet--object-buffer mirror)
    (format "mirror of %s %s"
            (snippet--field-name (snippet--mirror-source mirror))
            (if short "" (cl-call-next-method)))))

(cl-defmethod snippet--describe-object ((exit snippet--exit) &key short)
  (with-current-buffer (snippet--object-buffer exit)
    (format "exit %s" (if short "" (cl-call-next-method)))))

(defun snippet--debug-snippet (field-overlay)
  (with-current-buffer (get-buffer-create "*snippet-debug*")
    (let ((inhibit-read-only t)
          (sorted (cl-sort (cl-copy-list
                            (overlay-get field-overlay 'snippet--objects))
                           #'(lambda (f1 f2)
                               (let ((start1 (snippet--object-start f1))
                                     (start2 (snippet--object-start f2)))
                                 (if (< start1 start2)
                                   t
                                 (if (> start2 start1)
                                     nil
                                   (snippet--object-parent f2))))))))
      (erase-buffer)
      (cl-loop for object in sorted
               do (insert (snippet--describe-object object) "\n")))
    (display-buffer (current-buffer))))


(provide 'snippet)

;; Local Variables:
;; coding: utf-8
;; whitespace-mode: t
;; whitespace-style: (face lines-tail)
;; whitespace-line-column: 80
;; fill-column: 80
;; End:

;; snippet.el ends here
