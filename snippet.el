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
;; TODO: primary field transformations: the (&transform ...) option to &field
;;       constructs. In `define-dynamic-snippet', mirror forms should only be
;;       evaluated for value. &exit should be generalized to also allow nested
;;       fields.
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
  `(lambda (region-string)
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
        mirrors-and-sources
        all-objects)
    (when (> (cl-count '&exit unfolded :key #'car) 1)
      (error "Too many &exit forms given"))
    `(let* ((region-string (and (region-active-p)
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end))))
            ,@(loop
               for form in unfolded
               with mirror-idx = 0
               with sym
               with prev-sym
               append
               (pcase form
                 (`(&field ,name ,expr (&parent ,parent))
                  (setq sym (snippet--make-field-sym name))
                  `((,sym (snippet--make-object
                           'snippet--field
                           ,(pcase expr
                              (`(&eval ,form)
                               `(lambda (_ignored)
                                  (funcall
                                   ,(snippet--make-lambda form)
                                   region-string))))
                           :name ',name
                           :prev ,prev-sym
                           :parent ,parent))))
                 (`(&mirror ,name (&transform ,transform) (&parent ,parent))
                  (setq sym (snippet--make-mirror-sym
                             (cl-incf mirror-idx) name))
                  (push
                   (cons sym
                         (snippet--make-field-sym name))
                   mirrors-and-sources)
                  `((,sym (snippet--make-object
                           'snippet--mirror
                           nil
                           :transform ,(snippet--make-transform-lambda transform)
                           :prev ,prev-sym
                           :parent ,parent))))
                 (`(&exit (&eval ,form) (&parent ,parent))
                  (setq sym (snippet--make-exit-sym))
                  `((,sym (snippet--make-object
                           'snippet--exit
                           ,(and form
                                 `(funcall ,(snippet--make-lambda form)
                                           region-string))
                           :prev ,prev-sym
                           :parent ,parent))))
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
       ,@(cl-loop for (mirror . source) in mirrors-and-sources
                  collect `(setf (snippet--mirror-source ,mirror) ,source)
                  collect `(push ,mirror (snippet--field-mirrors ,source)))
       (snippet--activate-snippet (list ,@all-objects)))))

(def-edebug-spec snippet-form
  (&or
   ("&mirror" sexp def-form)
   ("&field" sexp &or ("&nested" &rest snippet-form) def-form)
   def-form))

(defmacro define-static-snippet (name _properties &optional docstring
                                      &rest snippet-forms)
  "Define NAME as an interactive snippet-inserting function.

Each form in SNIPPET-FORMS, inserted at point in order, can be:

* A cons (&field [FIELD-NAME [FIELD-DEFAULT]]) definining a snippet
  field. Snippet fields can be edited and be navigated to using
  `snippet-next-field' and `snippet-prev-field'. FIELD-NAME is
  optional and used for referring to the field in mirror
  transforms.

  FIELD-DEFAULT is optional and determines the field's starting
  text. It can be any lisp form returning a string or have be a cons
  (&transform FIELD-TRANSFORM) where FIELD-TRANSFORM is again a
  lisp form returning a string. Choosing between these varieties
  defines when and how these forms are evaluated. See below for
  details.

  FIELD-DEFAULT can also be a cons (&nested SUB-FORMS) where each
  form in SUB-FORMS is evaluated recursively according to the
  rules of SNIPPET-FORMS, thus producing nested fields and
  mirrors.

* A cons (&mirror FIELD-NAME [MIRROR-TRANSFORM]) defining a mirror
  of the field named FIELD-NAME. MIRROR-TRANSFORM is an optional
  form producing a string that becomes the mirror text. If it is
  not provided the mirror's text always matches FIELD-NAME's.

* Any lisp form producing a string, call it CONSTANT, that is a
  part of the snippet but constant while the snippet is alive.

* A cons (&exit [EXIT-DEFAULT]), defining a special field within
  the snippet where point should be placed when the snippet is
  exited normally. EXIT-DEFAULT has the same semantics as
  FIELD-DEFAULT.

The forms CONSTANT, FIELD-DEFAULT, MIRROR-TRANSFORM,
FIELD-TRANSFORM and EXIT-DEFAULT are all evaluated at
snippet-insertion-time and with the variable `region-string' set
to the text of the buffer selected at snippet-insertion time. If
no region was active the value of this variable is the empty
string.

The forms MIRROR-TRANSFORM and FIELD-TRANSFORM are additionally
also evaluated each time the field is changed. Moreover,
FIELD-TRANSFORM is also evaluated when the field is entered or
left. These forms are evaluated with the following variables set:

* `field-string' is set to the text contained in the field;
* `field-left' is non-nil if the field is being left;
* `field-entered' is non-nil if the field is being entered;
* `field-modified-p' is non-nil if the field has ever been modified
  since snippet creation

If the form CONSTANT returns nil or the empty string, it is
considered to have returned a single whitespace.

If FIELD-TRANSFORM returns a string, it replaces the fields text,
either at snippet-insertion-time or when the field is being
edited, entered or left. If FIELD-TRANSFORM returns nil at
snippet-insertion-time, it is considered to have returned the
empty string. If it returns nil on any other occasion, the field
is left unchanged.

If any other form returns nil, it is considered to have returned
the empty string.

PROPERTIES is an even-numbered property list of (KEY VAL)
pairs. Its meaning is not decided yet"
  (declare (debug (&define name sexp &rest snippet-forms))
           (indent defun))
  (unless (stringp docstring)
    (push docstring snippet-forms)
    (setq docstring nil))
  `(defun ,name () ,docstring
          (interactive)
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
                     `(let* ((fn (lambda () ,@field-forms))
                             (field
                              (snippet--make-object
                               'snippet--field
                               (lambda (fld)
                                 (setf snippet--prev-object fld)
                                 (let* ((snippet--current-field fld))
                                   (funcall fn)))
                               :name ',field-name
                               :parent snippet--current-field
                               :prev snippet--prev-object)))
                        (setf (gethash ',field-name snippet--fields)
                              field)
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
                             (mirror
                              (snippet--make-object
                               'snippet--mirror
                               nil
                               :transform fn
                               :prev snippet--prev-object
                               :parent snippet--current-field)))
                        (push mirror (gethash ',field-name snippet--mirrors))
                        (push mirror snippet--all-objects)
                        (setf snippet--prev-object mirror)))
                   (&exit ()
                     `(let ((exit (snippet--make-object
                                   'snippet--exit
                                   nil
                                   :prev snippet--prev-object
                                   :parent snippet--current-field)))
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
  "Define NAME as a snippet-inserting function.

Inside BODY the following local macros are available to
create snippet fields and mirrors:

* (&field NAME [FIELD-BODY]) declares the start of a snippet
  field at point. Snippet fields can be edited and be navigated
  to using `snippet-next-field' and
  `snippet-prev-field'. FIELD-BODY is a list of lisp forms which
  are evaluted for both return value and side-effects: Any text
  inserted becomes the field's starting text. If a string is
  returned, it is also inserted at the end of the field. Since
  FIELD-BODY is evaluated according to the rules of BODY, nested
  mirrors and fields become possible. Alternatively, FIELD-BODY
  can also be a single form, a cons (&transform
  FIELD-TRANSFORM-ARGS FIELD-TRANSFORM). See below for details.

* (&mirror FIELD-NAME [MIRROR-TRANSFORM-ARGS MIRROR-TRANSFORM])
  declares the start of a mirror at point. If
  MIRROR-TRANSFORM-ARGS and MIRROR-TRANSFORM are ommited the
  mirror's text always matches FIELD-NAME'S. If provided,
  MIRROR-TRANSFORM-ARGS is a list of 0 to 4 symbols that are
  automatically bound to values extracted from the snippet field
  FIELD-NAME and that can be used by the single form
  MIRROR-TRANSFORM to produce the mirror's text. The values
  passed in are, in order:

  * a string containing the field's text;
  * a boolean indicating if the field has just been entered;
  * a boolean indicating if the field has just been left;
  * a boolean indicating if the field has ever been modified since
    insertion

  MIRROR-TRANSFORM can also refer to any lexically available
  symbols established in by other forms in BODY.

* (&exit [EXIT-BODY]) declares a special field special within the
  snippet where point should be placed when the snippet is exited
  normally. EXIT-BODY has the same semantics as FIELD-BODY.

Except for these special cases, forms in BODY are evaluated
normally. If at least one snippet field was declared, a navigable
snippet is inserted at the end.

MIRROR-TRANSFORM is evaluated at snippet-insertion time and each
time the corresponding field changes to produce the mirror's
text. If it returns nil, it is considered to have returned the
empty string.

If FIELD-BODY has the form (&transform FIELD-TRANSFORM-ARGS
FIELD-TRANSFORM), FIELD-TRANSFORM is passed the same parameters
as MIRROR-TRANSFORM, except that at snippet-insertion time, the
value of the first parameter to FIELD-TRANSFORM-ARGS is nil. At
snippet-insertion time the field's starting text should be
produced. If it returns nil, it is considered to have returned
the empty string.

FIELD-TRANSFORM is also called every time the field is edited or
when it is entered or left. It may then return a string replacing
the field's text or return nil to leave it unchanged. If the
field is changed in this manner, any corresponding mirror's
MIRROR-TRANSFORMS are also evaluated at the time."
  (declare (debug (&define name sexp def-body))
           (indent defun))
  (let ((interactive nil))
    (unless (stringp docstring)
      (push docstring body)
      (setq docstring nil))
    (if (eq (car-safe (car body)) 'interactive)
        (setq interactive (pop body)))
    `(defun ,name ,args ,docstring
            ,@(when interactive
                `(,interactive))
            (with-dynamic-snippet ,@body))))

(def-edebug-spec &mirror (sexp &optional sexp &rest form))
(def-edebug-spec &field (sexp &rest form))

(put '&field 'lisp-indent-function 'defun)
(put '&mirror 'lisp-indent-function 'defun)
(put '&exit 'lisp-indent-function 'defun)


;;; Snippet mechanics
;;;

(defclass snippet--object ()
  ;; keep the two initargable slots on top otherwise everything breaks.
  ;; TODO: report this to Stefan or Eric
  ((parent :initarg :parent :reader snippet--object-parent)
   (prev :initarg :prev :accessor snippet--object-prev)
   (start :accessor snippet--object-start)
   (end :accessor snippet--object-end)
   (next :accessor snippet--object-next)
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

(defclass snippet--exit (snippet--field) ())

(defun snippet--object-< (o1 o2)
  (let ((start1 (snippet--object-start o1))
        (start2 (snippet--object-start o2)))
    (if (< start1 start2)
        t
      (if (> start2 start1)
          nil
        (snippet--object-parent o2)))))

(defun snippet--make-object (class fn &rest initargs)
  (let* ((object (apply #'make-instance class initargs))
         (prev (snippet--object-prev object)))
    (when prev
      (when (snippet--object-next prev)
        (error "previous object already has another sucessor"))
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
    (cond ((functionp fn)
           (let ((retval (funcall fn object)))
             (when (stringp retval) (insert retval))))
          ((stringp fn)
           (insert fn)))
    ;; Don't set the object's end if its already set and matches point. i.e. when
    ;; running its function some nested field might have set it already and, if
    ;; point hasn't moved since, we need both end markers to be the same object.
    (unless (and (snippet--object-end object)
                 (= (snippet--object-end object) (point)))
      (setf (snippet--object-end object)
            (point-marker)))
    (when (snippet--object-parent object)
      (setf (snippet--object-end
             (snippet--object-parent object))
            (snippet--object-end object)))
    (snippet--open-object object 'close)
    object))

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
    (mapc (lambda (m) (set-marker-insertion-type m nil)) stay)
    (mapc (lambda (m) (set-marker-insertion-type m t)) push)))

(defun snippet--call-with-current-object (object fn)
  (unwind-protect
      (progn
        (snippet--open-object object)
        (funcall fn))
    (snippet--open-object object 'close)))

(defmacro snippet--with-current-object (object &rest body)
  (declare (indent defun) (debug t))
  `(snippet--call-with-current-object ,object (lambda () ,@body)))

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
(defvar snippet--field-protection-overlays nil)
(make-variable-buffer-local 'snippet--field-overlay)
(make-variable-buffer-local 'snippet--field-protection-overlays)
(put 'snippet--field-overlay 'permanent-local t)
(put 'snippet--field-protection-overlays 'permanent-local t)

(defun snippet--move-to-field (field)
  (let ((start (snippet--object-start field))
        (end (snippet--object-end field)))
  (goto-char start)
  (move-overlay snippet--field-overlay start end)
  (move-overlay (car snippet--field-protection-overlays)
                (max (point-min) (1- start))
                start)
  (move-overlay (cadr snippet--field-protection-overlays)
                end
                (min (point-max) (1+ end)))
  (overlay-put snippet--field-overlay 'snippet--field field)))

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
    (cond ((and after?
                ;; Don't run if snippet is being exited
                (not (snippet--exiting-p))
                ;; Also don't run if in the middle of an undo
                (not undo-in-progress))
           
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

(defun snippet--field-protection-violated (_overlay after? _beg _end
                                                   &optional _pre-change-len)
  (unless (or after? undo-in-progress)
    (snippet-exit "protection overlay violated")))

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

(defface snippet--protection-overlay-face
  '((t (:background "tomato")))
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
        (snippet--move-to-field exit)
        (snippet-exit "exit")))))

(defun snippet-prev-field ()
  "Move the the start of the previous field in the current snippet.

Skips over nested fields if their parent has been modified."
  (interactive)
  (snippet-next-field t))

(defun snippet-exit (&optional reason)
  "Quit the currently active snippet."
  (interactive (list "interactive quit"))
  (cond (snippet--field-overlay
         (overlay-put snippet--field-overlay 'snippet--exit-reason reason)
         (unless (memq 'snippet--post-command-hook post-command-hook)
           (display-warning 'snippet "Hook not present: forced snippet cleanup")
           (snippet--cleanup reason)))
        (t
         (error "No active snippet"))))

(defun snippet--exiting-p ()
  
  (overlay-get snippet--field-overlay 'snippet--exit-reason))



;;; Main
;;;
(defvar snippet--debug nil)
;; (setq snippet--debug t)
;; (setq snippet--debug nil)

(defun snippet--activate-snippet (objects)
  (let* ((exit (or (cl-find-if #'snippet--exit-p objects)
                   (progn (push (snippet--make-object
                                 'snippet--exit
                                 nil
                                 ;; the first in `objects' must have been the
                                 ;; last inserted
                                 :prev (car objects)
                                 :parent nil
                                 :name 'snippet--exit-field)
                                objects)
                          (car objects))))
         (mirrors (cl-sort
                   (cl-copy-list
                    (cl-remove-if-not #'snippet--mirror-p objects))
                   #'(lambda (p1 p2)
                       (cond ((not p2) t)
                             ((not p1) nil)))
                   :key #'snippet--object-parent))
         (fields (cl-sort
                  (cl-copy-list
                   (cl-remove-if #'snippet--exit-p
                                 (cl-remove-if-not #'snippet--field-p objects)))
                  #'(lambda (n1 n2)
                      (cond ((not (integerp n2)) t)
                            ((not (integerp n1)) nil)
                            (t (< n1 n2))))
                  :key #'snippet--field-name)))
    (mapc #'snippet--update-mirror mirrors)
    (setq snippet--field-overlay
          (let ((overlay (make-overlay (point) (point) nil nil t)))
            (overlay-put overlay 'snippet--objects objects)
            (overlay-put overlay 'snippet--fields  fields)
            (overlay-put overlay 'snippet--exit    exit)
            (overlay-put overlay 'snippet--exit-reason nil)
            (overlay-put overlay 'face 'snippet-field-face)
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
    (setq snippet--field-protection-overlays
          (list (make-overlay (point) (point) nil nil nil)
                (make-overlay (point) (point) nil t nil)))
    (mapc #'(lambda (ov)
              (overlay-put ov
                           'modification-hooks
                           '(snippet--field-protection-violated))
              (overlay-put ov
                           'face (if snippet--debug
                                     'snippet--protection-overlay-face)))
          snippet--field-protection-overlays)
    (snippet-next-field)
    (add-hook 'post-command-hook 'snippet--post-command-hook 'append 'local)))

(defun snippet--cleanup (reason)
  (when snippet--field-overlay
    (delete-overlay snippet--field-overlay)
    (setq snippet--field-overlay nil))
  (when snippet--field-protection-overlays
    (mapc #'delete-overlay snippet--field-protection-overlays)
    (setq snippet--field-protection-overlays nil))
  (message "snippet exited (%s)" reason))

(defun snippet--post-command-hook ()
  ;; TODO: exiting the snippet might someday run user-provided code, hence the
  ;; apparent overengineeredness
  ;; 
  (let (remove-self exit-reason)
    (cond
     ((and snippet--field-overlay
           (not (overlay-buffer snippet--field-overlay)))
      ;; Something deleted the overlay 
      (setq remove-self t
            exit-reason "overlay destroyed"))
     (snippet--field-overlay
      (setq exit-reason
            (or (snippet--exiting-p)
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
    (if (and snippet--debug snippet--field-overlay) (snippet--debug-snippet))
    (when exit-reason
      (snippet--cleanup exit-reason))))


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

(defun snippet--debug-snippet ()
  (let ((inhibit-read-only t)
        (sorted (cl-sort (cl-copy-list
                          (overlay-get snippet--field-overlay
                                       'snippet--objects))
                         #'snippet--object-<))
        (buffer (current-buffer)))
    (with-current-buffer (get-buffer-create "*snippet-debug*")
      (erase-buffer)
      (cl-loop for object in sorted
               do (insert
                   (with-current-buffer buffer
                     (snippet--describe-object object))
                   "\n"))
      (display-buffer (current-buffer)))))


(provide 'snippet)

;; Local Variables:
;; coding: utf-8
;; whitespace-mode: t
;; whitespace-style: (face lines-tail)
;; whitespace-line-column: 80
;; fill-column: 80
;; End:

;; snippet.el ends here
