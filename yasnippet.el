;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;;
;; Author: pluskid <pluskid@gmail.com>
;; Version: 0.5.6 XXX: Change this
;; X-URL: http://code.google.com/p/yasnippet/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;;   1. Place `yasnippet.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'yasnippet)
;;   3. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   4. In your .emacs file
;;        (yas/initialize)
;;        (yas/load-directory "~/.emacs.d/snippets")
;;
;; For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/dont-activate nil
  "If set to t, don't activate yas/minor-mode automatically.")
(make-variable-buffer-local 'yas/dont-activate)

(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/root-directory nil
  "The (list of) root directory that stores the snippets for each
major modes.")

(defvar yas/indent-line t
  "Each (except the 1st) line of the snippet template is indented to
current column if this variable is non-`nil'.")
(make-variable-buffer-local 'yas/indent-line)

(defvar yas/trigger-key (kbd "<tab>")
  "The key to bind as a trigger of snippet.")
(defvar yas/next-field-key (kbd "<tab>")
  "The key to navigate to next field.")

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")
(define-key yas/keymap yas/next-field-key 'yas/next-field-group)
(define-key yas/keymap (kbd "S-TAB") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-iso-lefttab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-tab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<backtab>") 'yas/prev-field-group)

(defvar yas/show-all-modes-in-menu nil
  "Currently yasnippet only all \"real modes\" to menubar. For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'. There's really
no such mode like \"cc-mode\". So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes. The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t.")
(defvar yas/use-menu t
  "If this is set to `t', all snippet template of the current
mode will be listed under the menu \"yasnippet\".")
(defvar yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger.")

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight the currently active field of a snippet")

(defvar yas/window-system-popup-function #'yas/dropdown-list-popup-for-template
  "When there's multiple candidate for a snippet key. This function
is called to let user select one of them. `yas/text-popup-function'
is used instead when not in a window system.")
(defvar yas/text-popup-function #'yas/dropdown-list-popup-for-template
  "When there's multiple candidate for a snippet key. If not in a
window system, this function is called to let user select one of
them. `yas/window-system-popup-function' is used instead when in
a window system.")

(defvar yas/extra-mode-hooks
  '()
  "A list of mode-hook that should be hooked to enable yas/minor-mode.
Most modes need no special consideration.  Some mode (like `ruby-mode')
doesn't call `after-change-major-mode-hook' need to be hooked explicitly.")
(mapc '(lambda (x)
         (add-to-list 'yas/extra-mode-hooks
                      x))
      '(ruby-mode-hook actionscript-mode-hook ox-mode-hook python-mode-hook))

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.
The hooks will be run in an environment where some variables bound to
proper values:
 * yas/snippet-beg : The beginning of the region of the snippet.
 * yas/snippet-end : Similar to beg.")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run after a before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (not (bobp))
            (or (equal "font-lock-comment-face"
                       (get-char-property (1- (point))
                                          'face))
                (equal "font-lock-string-face"
                       (get-char-property (1- (point))
                                          'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Condition to yasnippet local to each buffer.

    * If yas/buffer-local-condition evaluate to nil, snippet
      won't be expanded.

    * If it evaluate to the a cons cell where the car is the
      symbol require-snippet-condition and the cdr is a
      symbol (let's call it requirement):
       * If the snippet has no condition, then it won't be
         expanded.
       * If the snippet has a condition but evaluate to nil or
         error occured during evaluation, it won't be expanded.
       * If the snippet has a condition that evaluate to
         non-nil (let's call it result):
          * If requirement is t, the snippet is ready to be
            expanded.
          * If requirement is eq to result, the snippet is ready
            to be expanded.
          * Otherwise the snippet won't be expanded.
    * If it evaluate to other non-nil value:
       * If the snippet has no condition, or has a condition that
         evaluate to non-nil, it is ready to be expanded.
       * Otherwise, it won't be expanded.

Here's an example:

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))")

(defvar yas/fallback-behavior 'call-other-command
  "The fall back behavior of YASnippet when it can't find a snippet
to expand.

 * 'call-other-command means try to temporarily disable
    YASnippet and call other command bound to `yas/trigger-key'.
 * 'return-nil means return nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/version "0.5.6")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")
(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major-mode.")
(defvar yas/menu-keymap (make-sparse-keymap "YASnippet"))
;; empty menu will cause problems, so we insert some items
(define-key yas/menu-keymap [yas/about]
  '(menu-item "About" yas/about))
(define-key yas/menu-keymap [yas/reload]
  '(menu-item "Reload all snippets" yas/reload-all))
(define-key yas/menu-keymap [yas/load]
  '(menu-item "Load snippets..." yas/load-directory))
(define-key yas/menu-keymap [yas/separator]
  '(menu-item "--"))

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")
(defconst yas/escape-backslash
  (concat "YASESCAPE" "BACKSLASH" "PROTECTGUARD"))
(defconst yas/escape-dollar
  (concat "YASESCAPE" "DOLLAR" "PROTECTGUARD"))
(defconst yas/escape-backquote
  (concat "YASESCAPE" "BACKQUOTE" "PROTECTGUARD"))

(defconst yas/field-regexp
  (concat "$\\([0-9]+\\)" "\\|"
          "${\\(?:\\([0-9]+\\):\\)?\\([^}]*\\)}"))

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")
(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

(defvar yas/overlay-modification-hooks
  (list 'yas/overlay-modification-hook)
  "The list of hooks to the overlay modification event.")
(defvar yas/overlay-insert-in-front-hooks
   (list 'yas/overlay-insert-in-front-hook)
   "The list of hooks of the overlay inserted in front event.")
(defvar yas/overlay-insert-behind-hooks
  (list 'yas/overlay-insert-behind-hook)
  "The list of hooks of the overlay inserted behind event.")


(setq yas/keymap-overlay-modification-hooks nil)

;; (defvar yas/keymap-overlay-modification-hooks
;;   (list 'yas/overlay-maybe-insert-behind-hook)
;;   "The list of hooks of the big keymap overlay modification event.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/minor-mode-map (make-sparse-keymap)
  "The keymap of yas/minor-mode")
(defvar yas/minor-mode-on-hook nil
  "Hook to call when yas/minor-mode is on.")
(defvar yas/minor-mode-off-hook nil
  "Hook to call when yas/minor-mode is off.")
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.
With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When YASnippet mode is enabled, the TAB key
expands snippets of code depending on the mode.

You can customize the key through `yas/trigger-key'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'editing
  (define-key yas/minor-mode-map yas/trigger-key 'yas/expand)

  (set 'yas/registered-snippets (make-hash-table :test 'eq)))

(defun yas/minor-mode-auto-on ()
  "Turn on YASnippet minor mode unless `yas/dont-activate' is
set to t."
  (unless yas/dont-activate
    (yas/minor-mode-on)))
(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode."
  (interactive)
  (yas/minor-mode 1))
(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition)))
  "A template for a snippet."
  content
  name
  condition)
(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

..."
  (groups nil)
  (exit-marker nil)
  (id (yas/snippet-next-id) :read-only t)
  (control-overlay nil)
  (active-field-overlay nil)
  (active-group nil)
  (end-marker nil)
  saved-active-group)

(defstruct (yas/group (:constructor yas/make-group (primary-field snippet)))
  "A group contains a list of field with the same number."
  primary-field
  (fields (list primary-field))
  (next nil)
  (prev nil)
  snippet
  (modified nil)
  saved-boundaries)
(defstruct (yas/field
            (:constructor yas/make-field (start end number value transform parent-field)))
  "A field in a snippet."
  start
  end
  number
  transform
  value
  parent-field
  subfields
  group)
(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (parent nil))

(defun yas/snippet-valid? (snippet)
  "See if snippet is valid (ie. still alive)."
  (and (not (null snippet))
       (not (null (yas/snippet-control-overlay snippet)))
       (not (null (overlay-start (yas/snippet-control-overlay snippet))))))

(defun yas/snippet-add-field (snippet field)
  "Add FIELD to the correct group of SNIPPET.

If no group is found, create one using `yas/make-group'. Return
FIELD."
  (let ((group (find field
                     (yas/snippet-groups snippet)
                     :test
                     '(lambda (field group)
                        (and (not (null (yas/field-number field)))
                             (not (null (yas/group-number group)))
                             (= (yas/field-number field)
                                (yas/group-number group)))))))
    (if group
        (yas/group-add-field group field)
      (setq group (yas/make-group field snippet))
      (push group (yas/snippet-groups snippet)))

    (setf (yas/field-group field) group))
  field)

(defun yas/group-value (group)
  "Get the default value of the field group."
  (or (yas/field-value
       (yas/group-primary-field group))
      ""))
(defun yas/group-number (group)
  "Get the number of the field GROUP."
  (yas/field-number
   (yas/group-primary-field group)))
(defun yas/group-add-field (group field)
  "Add a FIELD to the field GROUP. If the value of the primary
field is nil and that of the field is not nil, the field is set
as the primary field of the group."
  (push field (yas/group-fields group))
  (when (and (null (yas/field-value (yas/group-primary-field group)))
             (yas/field-value field))
    (setf (yas/group-primary-field group) field)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the field's start point"
  (let ((n1 (yas/field-number field1))
        (n2 (yas/field-number field2)))
    (if n1
        (if n2
            (< n1 n2)
          t)
      (if n2
          nil
        (< (yas/field-start field1)
           (yas/field-start field2))))))

(defun yas/template-condition-predicate (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition))))
    (error (progn
             (message (format "[yas]error in condition evaluation: %s"
                              (error-message-string err)))
             nil))))

(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the condition. The rules are:

 * If the template has no condition, it is kept.
 * If the template's condition eval to non-nil, it is kept.
 * Otherwise (eval error or eval to nil) it is filtered."
  (remove-if-not '(lambda (pair)
                    (let ((condition (yas/template-condition (cdr pair))))
                      (if (null condition)
                          (if yas/require-template-condition
                              nil
                            t)
                        (let ((result
                               (yas/template-condition-predicate condition)))
                          (if yas/require-template-condition
                              (if (eq yas/require-template-condition t)
                                  result
                                (eq result yas/require-template-condition))
                            result)))))
                 templates))

(defun yas/snippet-table-fetch (table key)
  "Fetch a snippet binding to KEY from TABLE. If not found,
fetch from parent if any."
  (let ((templates (yas/filter-templates-by-condition
                    (gethash key (yas/snippet-table-hash table)))))
    (when (and (null templates)
               (not (null (yas/snippet-table-parent table))))
      (setq templates (yas/snippet-table-fetch
                       (yas/snippet-table-parent table)
                       key)))
    templates))
(defun yas/snippet-table-store (table full-key key template)
  "Store a snippet template in the table."
  (puthash key
           (yas/modify-alist (gethash key
                                      (yas/snippet-table-hash table))
                             full-key
                             template)
           (yas/snippet-table-hash table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/ensure-minor-mode-priority ()
  "Ensure that the key binding of yas/minor-mode takes priority."
  (unless (eq 'yas/minor-mode
              (caar minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons
           (cons 'yas/minor-mode yas/minor-mode-map)
           (assq-delete-all 'yas/minor-mode
                            minor-mode-map-alist)))))

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/eval-string (string)
  "Evaluate STRING and convert the result to string."
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (format "%s" (eval (read string))))))
    (error (format "(error in elisp evaluation: %s)"
                   (error-message-string err)))))
(defun yas/calculate-field-value (field value)
  "Calculate the value of the field. If there's a transform
for this field, apply it. Otherwise, the value is returned
unmodified."
  (let ((text value)
        (transform (yas/field-transform field)))
    (if transform
        (yas/eval-string transform)
      text)))
(defsubst yas/replace-all (from to)
  "Replace all occurance from FROM to TO."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun yas/snippet-table (mode)
  "Get the snippet table corresponding to MODE."
  (let ((table (gethash mode yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table))
      (puthash mode table yas/snippet-tables))
    table))
(defsubst yas/current-snippet-table ()
  "Get the snippet table for current major-mode."
  (yas/snippet-table major-mode))

(defun yas/menu-keymap-for-mode (mode)
  "Get the menu keymap correspondong to MODE."
  (let ((keymap (gethash mode yas/menu-table)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (puthash mode keymap yas/menu-table))
    keymap))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end (point))
        (syntaxes yas/key-syntaxes)
        syntax done templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point)))
      (setq templates
            (yas/snippet-table-fetch
             (yas/current-snippet-table)
             (buffer-substring-no-properties start end)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))

(defun yas/update-mirrors (field-group &optional dont-recurse-down)
  "Update all mirror  fields' text according to the primary field."
  (when (yas/snippet-valid? (yas/group-snippet field-group))
    (save-excursion
      (let* ((inhibit-modification-hooks t)
             (primary (yas/group-primary-field field-group))
             (text (yas/current-field-text primary))
             (buffer-undo-list t))
        ;; For all fields except the primary, replace their text
        (yas/replace-fields-with-value (remove-if #'(lambda (field)
                                                      (equal field primary))
                                                  (yas/group-fields field-group))
                                       text)
        ;; Call recursively for subfields
        (unless dont-recurse-down
          (dolist (subfield (yas/field-subfields primary))
          (yas/update-mirrors (yas/field-group subfield))))
        ;; Call recursively for parent field
        (when (yas/field-parent-field primary)
          (yas/update-mirrors (yas/field-group (yas/field-parent-field primary))
                                  'dont-recurse))))))

(defun yas/current-field-text (field)
  (buffer-substring-no-properties (yas/field-start field)
                                  (yas/field-end field)))

(defun yas/current-active-group (&optional snippet point)
  "...

XXX: TODO: Remove if possible and replace inline.
"
  (let ((snippet (or snippet
                     (yas/snippet-of-current-keymap (or point
                                                        (point))))))
    (and snippet
         (yas/snippet-active-group snippet))))

(defun yas/overlay-modification-hook (overlay after? beg end &optional length)
  "Synchronizes all fields for the group of the current field overlay

Used to ensure mirror fields in the same group contain the same value
of the primary field."
  (when after?
    ;; (and after? (not undo-in-progress))
    (yas/update-mirrors (yas/current-active-group))))

(defun yas/overlay-insert-in-front-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted in front of a snippet field."
  (let ((group (yas/current-active-group)))
    (when (and after?
               group)
      (let ((inhibit-modification-hooks t))
        ;;
        ;; If the group hasn't ever been modified, delete its contents
        ;; completely.
        ;;
        (when (not (yas/group-modified group))
          (setf (yas/group-modified group) t)
          (when (> (overlay-end overlay) end)
            (save-excursion
              (goto-char end)
              (delete-char (- (overlay-end overlay) end))))
          ;; ;;
;;           ;; Mark subgroups as `yas/group-deleted', so we're no longer
;;           ;; able to move them. This action is undoable as long as
;;           ;; `yas/save-active-group-boundaries' exists in the `pre-command-hook'
;;           ;; in the proper place.
;;           ;;
;;           (mapcar #'(lambda (group)
;;                       (setf (yas/group-deleted group) t))
;;                   (mapcar #'yas/field-group (yas/field-subfields (yas/group-primary-field group))))
)
        ;; in any case, synchronize mirror fields
        (yas/update-mirrors group)))))

(defun yas/move-overlay-and-field (overlay field start end)
  (move-overlay overlay
                start
                end)
  (move-marker (yas/field-start field) start)
  (move-marker (yas/field-end field) end))

(defun yas/overlay-insert-behind-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted just behind the currently active field overlay."
  (let* ((group (yas/current-active-group))
         (field (and group
                     (yas/group-primary-field group))))
    (when (and after?
               field)
      (yas/move-overlay-and-field overlay field (overlay-start overlay) end)
      (yas/update-mirrors group))))

(defun yas/remove-recent-undo-from-history ()
  (let ((undo (car buffer-undo-list)))
    (while (null undo)
      (setq buffer-undo-list (cdr buffer-undo-list))
      (setq undo (car buffer-undo-list)))
    ;; Remove this undo operation record
    (setq buffer-undo-list (cdr buffer-undo-list))))

(defun yas/undo-expand-snippet (start key snippet)
  "Undo a snippet expansion. Delete the overlays. This undo can't be
redo-ed."
  (yas/remove-recent-undo-from-history)
  (let ((inhibit-modification-hooks t)
        (buffer-undo-list t))
    (yas/exit-snippet snippet)
    (goto-char start)
    (delete-char (- (yas/snippet-end-marker snippet)
                    start))
    (insert key)))

(defun yas/replace-fields-with-value (fields &optional rep)
"TODO: revise need for this rebuscatedeness."
  (dolist (field fields)
    (let* ((start (yas/field-start field))
           (end (yas/field-end field))
           (length (- end start))
           (text (yas/calculate-field-value field (or rep
                                                      (yas/field-value field))))
           (inhibit-modification-hooks t))
      (when text
        (goto-char start)
        (insert text)
        (delete-char length)
        (move-marker (yas/field-end field) (point))))))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)

  (goto-char start)

  (let ((key (buffer-substring-no-properties start end))
        (original-undo-list buffer-undo-list) ;; save previous undo information
        (inhibit-modification-hooks t)
        (length (- end start))
        (column (current-column)))
    (save-restriction
      (narrow-to-region start start)

      ;; (setq buffer-undo-list t) ;; disable undo for a short while
      (insert template)

      ;; Step 1: do necessary indent
      (when yas/indent-line
        (let* ((indent (if indent-tabs-mode
                           (concat (make-string (/ column tab-width) ?\t)
                                   (make-string (% column tab-width) ?\ ))
                         (make-string column ?\ ))))
          (goto-char (point-min))
          (while (and (zerop (forward-line))
                      (= (current-column) 0))
            (insert indent))))

      ;; Step 2: protect backslash and backquote
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)

      ;; Step 3: evaluate all backquotes
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`]*\\)`" nil t)
        ;; go back so that (current-column) in elisp code evaluation
        ;; will calculate to a meaningful value
        (goto-char (match-beginning 0))
        (replace-match (yas/eval-string (match-string-no-properties 1))
                       t t))

      ;; Step 4: protect all escapes, including backslash and backquot
      ;; which may be produced in Step 3
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)
      (yas/replace-all "\\$" yas/escape-dollar)

      ;; Step 5: Create and register a brand new snippet in the local
      ;; `yas/registered-snippets' var. Create fields.
      (let ((snippet (yas/register-snippet (yas/make-snippet))))
        (goto-char (point-min))
        (yas/field-parse-create snippet)

        ;; Step 6: Sort and link each field group
        (setf (yas/snippet-groups snippet)
              (sort (yas/snippet-groups snippet)
                    '(lambda (group1 group2)
                       (yas/snippet-field-compare
                        (yas/group-primary-field group1)
                        (yas/group-primary-field group2)))))
        (let ((prev nil))
          (dolist (group (yas/snippet-groups snippet))
            (setf (yas/group-prev group) prev)
            (when prev
              (setf (yas/group-next prev) group))
            (setq prev group)))

        ;; Step 7: Create keymap overlay for snippet
        (let ((overlay (make-overlay (point-min)
                                     (point-max)
                                     nil
                                     nil
                                     t)))
          (overlay-put overlay 'keymap yas/keymap)
          (overlay-put overlay 'yas/snippet-reference snippet)
          (setf (yas/snippet-control-overlay snippet) overlay)
          (setf (yas/snippet-end-marker snippet) (overlay-end overlay)))

        ;; Step 8: Replace mirror field values with primary group's
        ;; value
        (dolist (group (yas/snippet-groups snippet))
          (yas/replace-fields-with-value
           (remove-if #'(lambda (field)
                          (eq (yas/group-primary-field group) field))
                      (yas/group-fields group))
           (yas/group-value group)))

        ;; Step 9: restore all escape characters
        (yas/replace-all yas/escape-dollar "$")
        (yas/replace-all yas/escape-backquote "`")
        (yas/replace-all yas/escape-backslash "\\")

        ;; Step 11: move to end and make sure exit-marker exist
        (goto-char (point-max))
        (unless (yas/snippet-exit-marker snippet)
          (setf (yas/snippet-exit-marker snippet) (copy-marker (point) t)))

        ;; ;; Step 12: Construct undo information
        ;;         (unless (eq original-undo-list t)
        ;;           (add-to-list 'original-undo-list
        ;;                        `(apply yas/undo-expand-snippet
        ;;                                ,(point-min)
        ;;                                ,key
        ;;                                ,snippet)))

        ;; Step 13: remove the trigger key
        (widen)
        (delete-char length)

        ;; ;; Step 14: Restore undo information
        ;; (setq buffer-undo-list original-undo-list)

        ;; Step 15: place the cursor at a proper place
        (let* ((first-group (car (yas/snippet-groups snippet)))
               (first-field (and first-group
                                 (yas/group-primary-field first-group)))
              overlay)
          (cond (first-field
                 ;; Step 10: Move to the new group, setting up
                 ;; properties of the wandering active field overlay.
                 (yas/move-to-group snippet first-group))
                (t
                 ;; no need to call exit-snippet, since no overlay created.
                 (yas/exit-snippet snippet))))

        ;; Step 16: Do necessary indenting
        (save-excursion
          (goto-char (overlay-start (yas/snippet-control-overlay snippet)))
          (while (re-search-forward "$>" nil t)
            (replace-match "")
            (indent-according-to-mode)))))))

(defun yas/field-parse-create (snippet &optional parent-field)
  "Parse a recently inserted snippet template, creating all
necessary fields.

Allows nested placeholder in the style of Textmate."
  ;; 5. Search from current point for yas/field-regexp
  ;;
  ;; a) That is, look for "$<`number'>" or
  ;;    "${<`number'>:<`value'>}". A few special cases.
  ;;
  ;; b) When `value' starts with $, assume the rest is a lisp
  ;;    expression returning string. assign that to `transform'
  ;;
  ;;    A transformation is signalled when `value' starts with the
  ;;    character "$" as the first value after the ":". The rest of
  ;;    `value' is not allowed to have any other nested snippet
  ;;    definitions. Don't know if Textmate allows this...
  ;;
  ;; c) If `number' is 0 (zero) the string found is deleted and
  ;;    that special place is the snippet's exit marker...
  ;;
  ;; d) Otherwise a placeholder field for `number' is added to the
  ;;    snippet with `value' and `transform'.
  ;;
  ;; f) The enclosing "${<`number'>:" and closing bracket regions are
  ;;    delete.
  ;;
  ;; g) Then, still, buffer is temporarily narrowed down to `value'
  ;;    and `yas/field-parse-create' is called again recursively with
  ;;    the recently created field as `parent-field'. That might
  ;;    actually add more fields.
  ;;
  ;; h) Update `value' of the newly created field to adjust for some
  ;;    possible pruning that happened in the subcalls to
  ;;    `yas/field-parse-create'
  ;;
  ;;
  (while (re-search-forward yas/field-regexp nil t)
    (let* ((number (or (match-string-no-properties 1)
                       (match-string-no-properties 2)))
           (transform nil)
           (bracket-end (set-marker (make-marker)
                                    (yas/field-bracket-end)))
           (value-start (set-marker (make-marker) (match-beginning 3)))
           (value-end (set-marker (make-marker)
                                  (or (and (marker-position bracket-end)
                                           (1- bracket-end))
                                      (match-end 3))))
           (value (when (and (marker-position value-start)
                             (marker-position value-end))
                    (buffer-substring-no-properties value-start value-end)))
           brand-new-field)
      ;; b) look for a transformation
      (when (eq (elt value 0) ?\$)
        (setq transform (substring value 1))
        (setq value nil))
      (if (and number
               (string= "0" number))
          ;; c) set exit marker and forget
          (progn
            (replace-match "")
            (setf (yas/snippet-exit-marker snippet)
                  (copy-marker (point) t)))
        ;; d) add a brand new field, linking it to the possible parent
        ;; field and adding it to the parent field's subfield list.
        (setq brand-new-field
              (yas/snippet-add-field
               snippet
               (yas/make-field
                (set-marker (make-marker) (match-beginning 0))
                (set-marker (make-marker) (or (marker-position bracket-end)
                                              (match-end 0)))
                (and number (string-to-number number))
                value
                transform
                parent-field)))
        (when parent-field
          (setf (yas/field-subfields parent-field)
                (push brand-new-field (yas/field-subfields parent-field))))

        ;; f) delete useless regions, move to correct spot for more
        ;; search...
        (delete-region (match-beginning 0) (or (marker-position value-start)
                                               (point)))
        (when value
          (when (marker-position bracket-end)
            (delete-region value-end bracket-end))

          ;; g) investigate nested placeholders
          (save-excursion
            (save-restriction
              (narrow-to-region value-start value-end)
              (goto-char (point-min))
              (yas/field-parse-create snippet brand-new-field)))
          ;; h)
          (setf (yas/field-value brand-new-field)
                (buffer-substring-no-properties value-start value-end)))))))

(defun yas/field-bracket-end ()
  "Calculates position of the field's closing bracket if any.

Assumes a regexp search for `yas/field-regexp' matched
recently. Return Nil if no field value subexpression was found,
or throws error if the snippet has malformed nested
placeholders."
  (let ((bracket-or-number-start (1+ (match-beginning 0)))
        bracket-end)
    (when (eq ?\{ (char-after bracket-or-number-start))
      (setq bracket-end (condition-case oops
                            (scan-sexps bracket-or-number-start 1)
                          ;; TODO: Later should throw another error with
                          ;; information about failed syntax!
                          (error
                           (message "Invalid snippet template!")))))
    bracket-end))

(defun yas/snippet-of-current-keymap (&optional point)
  "Return the most recently inserted snippet covering POINT."
  (let ((point (or point (point)))
        (keymap-snippet nil)
        (snippet nil))
    (dolist (overlay (overlays-at point))
      (setq snippet (overlay-get overlay 'yas/snippet-reference))
      (when snippet
        (if (null keymap-snippet)
            (setq keymap-snippet snippet)
          (when (> (yas/snippet-id snippet)
                   (yas/snippet-id keymap-snippet))
            (setq keymap-snippet snippet)))))
    keymap-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template-related and snippet loading functions

(defun yas/parse-template (&optional file-name)
  "Parse the template in the current buffer.
If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Here's a list of currently recognized variables:

 * name
 * contributor
 * condition

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let ((name file-name) template bound condition)
    (if (re-search-forward "^# --\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^#\\([^ ]+\\) *: *\\(.*\\)$" bound t)
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (read (match-string-no-properties 2))))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (list template name condition)))

(defun yas/directory-files (directory file?)
  "Return directory files or subdirectories in full path."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  (lexical-let ((template template))
    (lambda ()
      (interactive)
      (yas/expand-snippet (point)
                          (point)
                          template))))

(defun yas/modify-alist (alist key value)
  "Modify ALIST to map KEY to VALUE. return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
        (cons (cons key value)
              alist)
      (setcdr pair value)
      alist)))

(defun yas/fake-keymap-for-popup (templates)
  "Create a fake keymap for popup menu usage."
  (cons 'keymap
        (mapcar (lambda (pair)
                  (let* ((template (cdr pair))
                         (name (yas/template-name template))
                         (content (yas/template-content template)))
                    (list content 'menu-item name t)))
                templates)))

(defun yas/point-to-coord (&optional point)
  "Get the xoffset/yoffset information of POINT.
If POINT is not given, default is to current point.
If `posn-at-point' is not available (like in Emacs 21.3),
t is returned simply."
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (or point (point))))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))

(defun yas/x-popup-menu-for-template (templates)
  "Show a popup menu listing templates to let the user select one."
  (car (x-popup-menu (yas/point-to-coord)
                     (yas/fake-keymap-for-popup templates))))
(defun yas/text-popup-for-template (templates)
  "Can't display popup menu in text mode. Just select the first one."
  (yas/template-content (cdar templates)))
(defun yas/dropdown-list-popup-for-template (templates)
  "Use dropdown-list.el to popup for templates. Better than the
default \"select first\" behavior of `yas/text-popup-for-template'.
You can also use this in window-system.

NOTE: You need to download and install dropdown-list.el to use this."
  (if (fboundp 'dropdown-list)
      (let ((n (dropdown-list (mapcar (lambda (i)
                                        (yas/template-name
                                         (cdr i)))
                                      templates))))
        (if n
            (yas/template-content
             (cdr (nth n templates)))
          nil))
    (error "Please download and install dropdown-list.el to use this")))

(defun yas/popup-for-template (templates)
  (if window-system
      (funcall yas/window-system-popup-function templates)
    (funcall yas/text-popup-function templates)))

(defun yas/load-directory-1 (directory &optional parent)
  "Really do the job of loading snippets from a directory
hierarchy."
  (let ((mode-sym (intern (file-name-nondirectory directory)))
        (snippets nil))
    (with-temp-buffer
      (dolist (file (yas/directory-files directory t))
        (when (file-readable-p file)
          (insert-file-contents file nil nil nil t)
          (let ((snippet-file-name (file-name-nondirectory file)))
            (push (cons snippet-file-name
                        (yas/parse-template snippet-file-name))
                  snippets)))))
    (yas/define-snippets mode-sym
                         snippets
                         parent)
    (dolist (subdir (yas/directory-files directory nil))
      (yas/load-directory-1 subdir mode-sym))))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.
SNIPPET-ROOTS is a list of root directories that contains the snippets
definition. YASNIPPET is the yasnippet.el file path. YASNIPPET-BUNDLE
is the output file of the compile result. CODE is the code you would
like to used to initialize yasnippet. Here's the default value for
all the parameters:

 (yas/compile-bundle \"yasnippet.el\"
                     \"./yasnippet-bundle.el\"
                     '(\"snippets\")
                     \"(yas/initialize)\")"
  (when (null yasnippet)
    (setq yasnippet "yasnippet.el"))
  (when (null yasnippet-bundle)
    (setq yasnippet-bundle "./yasnippet-bundle.el"))
  (when (null snippet-roots)
    (setq snippet-roots '("snippets")))
  (when (null code)
    (setq code "(yas/initialize)"))

  (let ((dirs (or (and (listp snippet-roots) snippet-roots)
                  (list snippet-roots)))
        (bundle-buffer nil))
    (with-temp-buffer
      (setq bundle-buffer (current-buffer))
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert code "\n")
      (flet ((yas/define-snippets
              (mode snippets &optional parent)
              (with-current-buffer bundle-buffer
                (insert ";;; snippets for " (symbol-name mode) "\n")
                (insert "(yas/define-snippets '" (symbol-name mode) "\n")
                (insert "'(\n")
                (dolist (snippet snippets)
                  (insert "  ("
                          (yas/quote-string (car snippet))
                          " "
                          (yas/quote-string (cadr snippet))
                          " "
                          (if (caddr snippet)
                              (yas/quote-string (caddr snippet))
                            "nil")
                          " "
                          (if (nth 3 snippet)
                              (format "'%s" (nth 3 snippet))
                            "nil")
                          ")\n"))
                (insert "  )\n")
                (insert (if parent
                            (concat "'" (symbol-name parent))
                          "nil")
                        ")\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/directory-files dir nil))
            (yas/load-directory-1 subdir nil))))
      (insert "(provide '"
              (file-name-nondirectory
               (file-name-sans-extension
                yasnippet-bundle))
              ")\n")
      (setq buffer-file-name yasnippet-bundle)
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>")))
(defun yas/reload-all ()
  "Reload all snippets."
  (interactive)
  (if yas/root-directory
      (if (listp yas/root-directory)
          (dolist (directory yas/root-directory)
            (yas/load-directory directory))
        (yas/load-directory yas/root-directory))
    (call-interactively 'yas/load-directory))
  (message "done."))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.
Below the top-level directory, each directory is a mode
name.  And under each subdirectory, each file is a definition
of a snippet.  The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless (file-directory-p directory)
    (error "Error %s not a directory" directory))
  (add-to-list 'yas/root-directory directory)
  (dolist (dir (yas/directory-files directory nil))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "done.")))

(defun yas/initialize ()
  "Do necessary initialization."
  (add-hook 'after-change-major-mode-hook
            'yas/minor-mode-auto-on)
  (dolist (hook yas/extra-mode-hooks)
    (add-hook hook
              'yas/minor-mode-auto-on))
  (add-hook 'yas/minor-mode-on-hook
            'yas/ensure-minor-mode-priority)
  (when yas/use-menu
    (define-key-after
      (lookup-key global-map [menu-bar])
      [yasnippet]
      (cons "YASnippet" yas/menu-keymap)
      'buffer)))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define snippets for MODE.  SNIPPETS is a list of
snippet definition, of the following form:

 (KEY TEMPLATE NAME CONDITION)

or the NAME and CONDITION may be omitted.  The optional 3rd
parameter can be used to specify the parent mode of MODE.  That
is, when looking a snippet in MODE failed, it can refer to its
parent mode.  The PARENT-MODE may not need to be a real mode."
  (let ((snippet-table (yas/snippet-table mode))
        (parent-table (if parent-mode
                          (yas/snippet-table parent-mode)
                        nil))
        (keymap (if yas/use-menu
                    (yas/menu-keymap-for-mode mode)
                  nil)))
    (when parent-table
      (setf (yas/snippet-table-parent snippet-table)
            parent-table)
      (when yas/use-menu
        (define-key keymap (vector 'parent-mode)
          `(menu-item "parent mode"
                      ,(yas/menu-keymap-for-mode parent-mode)))))
    (when (and yas/use-menu
               (yas/real-mode? mode))
      (define-key yas/menu-keymap (vector mode)
        `(menu-item ,(symbol-name mode) ,keymap)))
    (dolist (snippet snippets)
      (let* ((full-key (car snippet))
             (key (file-name-sans-extension full-key))
             (name (or (caddr snippet) (file-name-extension full-key)))
             (condition (nth 3 snippet))
             (template (yas/make-template (cadr snippet)
                                          (or name key)
                                          condition)))
        (yas/snippet-table-store snippet-table
                                 full-key
                                 key
                                 template)
        (when yas/use-menu
          (define-key keymap (vector (make-symbol full-key))
            `(menu-item ,(yas/template-name template)
                        ,(yas/make-menu-binding (yas/template-content template))
                        :keys ,(concat key yas/trigger-symbol))))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
         (yas/snippet-table mode))
        (yas/snippet-table parent))
  (when yas/use-menu
    (define-key (yas/menu-keymap-for-mode mode) (vector 'parent-mode)
      `(menu-item "parent mode"
                  ,(yas/menu-keymap-for-mode parent)))))

(defun yas/define (mode key template &optional name condition)
  "Define a snippet.  Expanding KEY into TEMPLATE.
NAME is a description to this template.  Also update
the menu if `yas/use-menu' is `t'.  CONDITION is the
condition attached to this snippet.  If you attach a
condition to a snippet, then it will only be expanded
when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition))))


(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (when (and (null (car buffer-undo-list))
               (eq 'apply
                   (car (cadr buffer-undo-list)))
               (eq 'yas/undo-expand-snippet
                   (cadr (cadr buffer-undo-list))))
      (undo 1))
    nil))

(defun yas/expand ()
  "Expand a snippet."
  (interactive)
  (let ((local-condition (yas/template-condition-predicate
                          yas/buffer-local-condition)))
    (if local-condition
        (let ((yas/require-template-condition
               (if (and (consp local-condition)
                        (eq 'require-snippet-condition (car local-condition))
                        (symbolp (cdr local-condition)))
                   (cdr local-condition)
                 nil)))
          (multiple-value-bind (templates start end) (yas/current-key)
            (if templates
                (let ((template (if (null (cdr templates)) ; only 1 template
                                    (yas/template-content (cdar templates))
                                  (yas/popup-for-template templates))))
                  (if template
                      (progn (yas/expand-snippet start end template)
                             'expanded) ; expanded successfully
                    'interrupted))     ; interrupted by user
              (if (eq yas/fallback-behavior 'return-nil)
                  nil                   ; return nil
                (let* ((yas/minor-mode nil)
                       (command (key-binding yas/trigger-key)))
                  (when (commandp command)
                    (call-interactively command))))))))))

(defun yas/current-group-for-navigation (&optional snippet)
  (and snippet
       (yas/snippet-active-group snippet)))

(defun yas/group-probably-deleted-p (group)
  (let ((primary-field (yas/group-primary-field group)))
    (and (zerop (- (yas/field-start primary-field) (yas/field-end primary-field)))
         (yas/field-parent-field primary-field))))

(defun yas/next-field-group (&optional arg)
  "Navigate to next field group.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (yas/snippet-of-current-keymap))
         (number (and snippet
                      (+ arg
                         (yas/group-number (yas/current-group-for-navigation snippet)))))
         (live-groups (remove-if #'yas/group-probably-deleted-p (yas/snippet-groups snippet)))
         (target-group (and number
                            (> number 0)
                            (find-if #'(lambda (group)
                                         (= number (yas/group-number group)))
                                     live-groups))))
    (cond ((and number
                (> number (length live-groups)))
           (yas/exit-snippet snippet))
          (target-group
           (yas/move-to-group snippet target-group))
          (t
           nil))))

(defun yas/move-to-group (snippet group &optional dontmove)
  "Update SNIPPET to move to group GROUP."
  (let ((field (yas/group-primary-field group))
        (overlay (yas/snippet-active-field-overlay snippet)))
    (unless dontmove
      (goto-char (yas/field-start field)))
    (setf (yas/snippet-active-group snippet) group)
;;    (setf (yas/group-deleted group) nil)
    (cond ((and overlay
                (overlay-buffer overlay))
           (move-overlay overlay (yas/field-start field)
                         (yas/field-end field)))
          (t
           (setq overlay (make-overlay (yas/field-start first-field) (yas/field-end first-field)))
           (overlay-put overlay 'modification-hooks yas/overlay-modification-hooks)
           (overlay-put overlay 'insert-in-front-hooks yas/overlay-insert-in-front-hooks)
           (overlay-put overlay 'insert-behind-hooks yas/overlay-insert-behind-hooks)
           (overlay-put overlay 'face 'yas/field-highlight-face)
           (setf (yas/snippet-active-field-overlay snippet) overlay)))))


(defun yas/prev-field-group ()
  "Navigate to prev field group.  If there's none, exit the snippet."
  (interactive)
  (yas/next-field-group -1))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and cleanup the snippet.  Cleaning
up the snippet does not delete it!"
  (interactive)
  (goto-char (yas/snippet-exit-marker snippet))
  (yas/cleanup-snippet snippet))

;; Snippet register and unregister routines.
;;
;; XXX: Commentary on this section by joaot.
;;
;; These routines, along with minor modifications upwards, allow some
;; management of currently active snippets.
;;
;; The idea is to temporarily set `post-command-hook' while locally
;; "registered" snippets last.  After each command,
;; `yas/check-cleanup-snippet' is run, checking for some condition and
;; possibly unregistering the snippet.  When no more snippets are
;; registered, the `post-command-hook' is cleared up.
;;
;; They were introduced to fix bug 28
;; "http://code.google.com/p/yasnippet/issues/detail?id=28".  Whenever
;; point exits a snippet or a snippet field, *all* snippets are
;; destroyed.
;;
;; Also, this scheme have been reused to fix bug 33
;; "http://code.google.com/p/yasnippet/issues/detail?id=33", which
;; deals with undoing changes when part of the snippet's field have
;; been filled out already.  See commentary on "Field-level undo" below
;;

(defvar yas/registered-snippets nil
  "A hash table holding all active snippets")
(eval-when-compile
  (make-variable-buffer-local 'yas/registered-snippets))

(defun yas/register-snippet (snippet)
  "Register SNIPPET in the `yas/registered-snippets' table.  Add a
`yas/check-cleanup-snippet' function to the buffer-local
`post-command-hook' that should exist while at least one
registered snippet exists in the current buffer.  Return snippet"
  (puthash (yas/snippet-id snippet) snippet yas/registered-snippets)
  (add-hook 'pre-command-hook  'yas/save-active-group-boundaries            'append 'local)
  (add-hook 'post-command-hook 'yas/save-active-group-boundaries-after             'append 'local)
  (add-hook 'post-command-hook 'yas/check-cleanup-snippet       'append 'local)
  ;; DEBUG
  (add-hook 'post-command-hook 'yas/debug-some-vars             'append 'local)
  snippet)

(defun yas/unregister-snippet (snippet)
  "Unregister snippet from the `yas/registered-snippets'
table.  Remove `yas/check-cleanup-snippet' from the buffer-local
`post-command-hook' if no more snippets registered in the
current buffer."
  (remhash (yas/snippet-id snippet) yas/registered-snippets)
  (when (eq 0
            (hash-table-count yas/registered-snippets))
    (remove-hook 'pre-command-hook  'yas/save-active-group-boundaries      'local)
    (remove-hook 'post-command-hook 'yas/save-active-group-boundaries-after       'local)
    (remove-hook 'post-command-hook 'yas/check-cleanup-snippet 'local)
    ;; DEBUG
    (remove-hook 'post-command-hook 'yas/debug-some-vars       'local)))

(defun yas/exterminate-snippets ()
  "Remove all locally registered snippets and remove
  `yas/check-cleanup-snippet' from the `post-command-hook'"
  (interactive)
  (maphash #'(lambda (key snippet)
               (when (yas/snippet-p snippet) (yas/cleanup-snippet snippet)))
           yas/registered-snippets)
  (unless (eq 0 (hash-table-count yas/registered-snippets))
    (setq yas/registered-snippets (make-hash-table :test 'eq))
    (message "Warning: yas/snippet hash-table not fully clean. Forcing NIL.")))

(defun yas/cleanup-snippet (snippet)
  "Cleanup SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text"
  (let* ((control-overlay (yas/snippet-control-overlay snippet))
         (field-overlay (yas/snippet-active-field-overlay snippet))
         yas/snippet-beg yas/snippet-end)
    ;;
    ;; Save the end of the moribund snippet in case we need to undo
    ;; its original expansion.  This is used by `yas/undo-expand-snippet'
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas/snippet-beg (overlay-start control-overlay))
      (setq yas/snippet-end (overlay-end control-overlay))
      (setf (yas/snippet-end-marker snippet) yas/snippet-end)
      (delete-overlay control-overlay))
    ;;
    ;; Delete the currently active field overlay if any
    ;;
    (when (and field-overlay
               (overlay-buffer field-overlay))
      (delete-overlay field-overlay))
    ;;
    ;; Iterate every group, and in it, every field.
    ;;
    (dolist (group (yas/snippet-groups snippet))
      (dolist (field (yas/group-fields group))
        (let ((start-marker (yas/field-start field))
              (end-marker (yas/field-end field)))
          ;;
          ;; convert markers into points, before losing the reference.
          ;;
          (when (markerp start-marker)
            (setf (yas/field-start field) (marker-position start-marker))
            (set-marker start-marker nil))
          (when (markerp end-marker)
            (setf (yas/field-end field) (marker-position end-marker))
            (set-marker end-marker nil)))))
    ;;
    ;; XXX: `yas/after-exit-snippet-hook' should be run with
    ;; `yas/snippet-beg' and `yas/snippet-end' bound. That might not
    ;; be the case if the main overlay had somehow already
    ;; disappeared, which sometimes happens when the snippet's messed
    ;; up...
    ;;
    (run-hooks 'yas/after-exit-snippet-hook))
  (yas/unregister-snippet snippet))

(defun yas/check-cleanup-snippet ()
  "Checks if point exited the currently active field of the
snippet, if so cleans up the whole snippet up.

This function is part of `post-command-hook' while
registered snippets last."
  (let* ((snippet (yas/snippet-of-current-keymap))
         (group (and snippet
                     (yas/snippet-active-group snippet))))
    (cond (;;
           ;; No snippet at point, cleanup *all* snippets
           ;;
           (null snippet)
           (yas/exterminate-snippets))
          (;; A snippet exits at point, but point left the currently
           ;; active field overlay
           (or (not group)
               (and group
                    (not (yas/point-in-field-p (yas/group-primary-field group)))))
           (yas/cleanup-snippet snippet))
          (;;
           ;; Snippet at point, and point inside a snippet field,
           ;; everything is normal
           ;;
           t
           nil))))

;; Field-level undo functionality
;;
;; XXX: Commentary on this section by joaot.
;;
;; ...

(defun yas/save-active-group-boundaries ()
  "While snippet is active, save the active group and the active group's boundaries.

This is stored in the `yas/group' itself.

Intended to be placed in `pre-command-hook'."
  (let* ((snippet (yas/snippet-of-current-keymap))
	 (group (yas/snippet-active-group snippet))
	 (field-overlay (yas/snippet-active-field-overlay snippet)))
    ;;
    ;; Save a reference to current group
    ;;
    (setf (yas/snippet-saved-active-group snippet)
	  group)
    ;;
    ;; Save boundaries of current field
    ;;
    (setf (yas/group-saved-boundaries group)  
	  (cons (overlay-start field-overlay)
		(overlay-end field-overlay)))))

(defun yas/save-active-group-boundaries-after ()
  "..."
  (let* ((snippet (yas/snippet-of-current-keymap))
         (saved-group (yas/snippet-saved-active-group snippet))
	 (saved-boundaries (yas/group-saved-boundaries saved-group)))
    ;;
    ;; Push an action to restore the active group
    ;; 
    (yas/push-undo-action-maybe (list 'yas/restore-active-group
				      saved-group))
    ;;
    ;; Push an action after that to restore the active group's
    ;; boundaries
    ;;
    (yas/push-undo-action-maybe (list 'yas/restore-active-group-boundaries
				      (car saved-boundaries)
				      (cdr saved-boundaries)))))

(defun yas/restore-active-group (group)
  "..."
  (let* ((snippet (yas/snippet-of-current-keymap))
         (inhibit-modification-hooks t))
    (yas/move-to-group snippet group 'dontmove)))

(defun yas/restore-active-group-boundaries (group)
  ",,,"
  (let* ((snippet (yas/snippet-of-current-keymap))
	 (group (yas/snippet-active-group snippet))
	 (field-overlay (yas/snippet-active-field-overlay snippet))
         (field (yas/group-primary-field group))
	 (inhibit-modification-hooks t))
    (yas/move-overlay-and-field field-overlay field start end)
    (yas/update-mirrors group)))

(defun yas/point-in-field-p (field &optional point)
  "..."
  (let ((point (or point
                   (point))))
    (and (>= point (yas/field-start field))
         (<= point (yas/field-end field)))))

(defun yas/push-undo-action-maybe (apply-args)
  "..."
  (let ((undo-list buffer-undo-list)
        (target-separator nil)
        done)
    (unless (eq t buffer-undo-list)
      ;;
      ;; Discard possibly existing/missing start separator
      ;;
      (when (null (car undo-list))
        (setq undo-list (cdr undo-list)))
      ;;
      ;; Find the target separator keeping `undo-list' as a reference to
      ;; the list starting before that.
      ;;
      (while (not done)
        (cond ((eq (first apply-args)
                   (condition-case opps
                       (second (car undo-list))
                     (error nil)))
               (setq done 'return))
              ((null (cadr undo-list))
               (setq done 'try-insert))
              (t
               (setq undo-list (cdr undo-list)))))
      (unless (eq done 'return)
        ;;
        ;; Push a the apply-args action there
        ;;
        (setq target-separator (cdr undo-list))
        (setf (cdr undo-list)
              (cons (cons 'apply
                          apply-args)
                    target-separator))))))

(defun yas/sanitize-undo-redo ()
  (let ((undo-list buffer-undo-list)
        done)
    (unless (eq t buffer-undo-list)
      ;;
      ;; Discard possibly existing/missing start separator
      ;;
      (when (null (car undo-list))
        (setq undo-list (cdr undo-list)))
      (delete-if #'(lambda (elem)
                     (when (and (consp elem)
                                (integerp (cdr elem))
                                (> (cdr elem) (point-max)))
                       (prog1 t
                         (message "Deleting %s in the undo-list (greater than point-max=%s)!!!" elem (point-max)))))
                 undo-list
                 :end (position nil undo-list)))))


;; Debug functions.  Use (or change) at will whenever needed.

(defun yas/debug-some-vars ()
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
    (princ "Interesting YASnippet vars: \n\n")
    (princ (format "Register hash-table: %s\n\n" yas/registered-snippets))
    (cond ((not yas/registered-snippets)
           (princ "  No snippet hash table!"))
          ((eq (hash-table-count yas/registered-snippets) 0)
           (princ "  No registered snippets\n"))
          (t
           (maphash #'(lambda (key snippet)
                        (princ (format "\t key %s for snippet %s"
                                       key
                                       (yas/snippet-id snippet)))


                        (princ (format "\t   Big overlay %s\n"
                                       (yas/snippet-control-overlay snippet)))

                        (if (yas/snippet-active-field-overlay snippet)
                            (princ (format "\t    Field overlay %s\n "
                                           (yas/snippet-active-field-overlay snippet)))
                          (princ "No active field overlay!!\m"))


                        (dolist (group (yas/snippet-groups snippet))
                          (princ (format "\t  Group $%s with %s fields is %s and %s"
                                         (yas/group-number group)
                                         (length (yas/group-fields group))
                                         (if (yas/group-probably-deleted-p group)
                                             "DELETED"
                                           "alive")
                                         (if (eq group (yas/snippet-active-group snippet))
                                             "ACTIVE!\n"
                                           "NOT ACTIVE!\n")))
                          (dolist (field (yas/group-fields group))
                            (princ (format "\t\t* %s field. Current value (%s) .\n"
                                           (if (eq field (yas/group-primary-field group))
                                               "Primary" "Mirror")
                                           (yas/current-field-text field)))
                            (princ (format "\t\t  From %s to %s\n"
                                           (yas/field-start field)
                                           (yas/field-end field)))
                            ))) yas/registered-snippets)))

    (princ (format "\nPost command hook: %s\n" post-command-hook))
    (princ (format "\nPre  command hook: %s\n" pre-command-hook))

    (princ (format "\nUndo is %s and point-max is %s.\n"
                   (if (eq buffer-undo-list t)
                       "DISABLED"
                     "ENABLED")
                   (point-max)))
    (unless (eq buffer-undo-list t)
      (princ (format "Undolist has %s elements. First 10 elements follow:\n" (length buffer-undo-list)))
      (let ((first-ten (subseq buffer-undo-list 0 19)))
        (dolist (undo-elem first-ten)
          (princ (format "%s:  %s\n" (position undo-elem first-ten) (truncate-string-to-width (format "%s" undo-elem) 50))))))))

(defun yas/exterminate-package ()
  (interactive)
  (yas/minor-mode -1)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))
