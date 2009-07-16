;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid

;; Authors: pluskid <pluskid@gmail.com>, joaotavora <joaotavora@gmail.com>
;; Version: 0.6.0
;; X-URL: http://code.google.com/p/yasnippet/
;; Keywords: snippet, textmate
;; URL: http://code.google.com/p/yasnippet/
;; EmacsWiki: YaSnippetMode

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
;;	  (add-to-list 'load-path "/dir/to/yasnippet.el")
;;   2. In your .emacs file:
;;        (require 'yasnippet)
;;   3. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   4. In your .emacs file
;;        (setq yas/root-directory "~/.emacs/snippets") 
;;        (yas/load-directory yas/root-directory)
;;   5. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   6. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 5. and 6. are optional, you can insert use snippets without
;;   them via:
;;        M-x yas/choose-snippet
;;
;;   The `dropdown-list.el' extension is bundled with YASnippet, you
;;   can optionally use it the preferred "prompting method", puting in
;;   your .emacs file, for example:
;;
;;       (require 'dropdown-list)
;;       (setq 'yas/prompt-functions '(yas/dropdown-prompt
;;				       yas/ido-prompt
;;				       yas/completing-prompt))
;;
;;   Also check out the customization group
;;        M-x customize-group RET yasnippet RET 
;;
;; For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;

(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :group 'editing)

(defcustom yas/root-directory nil
  "The (list of) root directory that stores the snippets for each
major mode."
  :type '(string)
  :group 'yasnippet)

(defcustom yas/prompt-functions '(yas/x-prompt
				  yas/ido-prompt
				  yas/dropdown-prompt
				  yas/completing-prompt
				  yas/no-prompt)
  "List of functions to prompt the user for keys, templates and
other values interactively."
  :type 'list
  :group 'yasnippet)

(defcustom yas/indent-line 'auto
  "Controls indenting applied to a recent snippet expansion.

The following values are possible:

`fixed' Indent the snippet to the current column;

`auto' Indent each line of the snippet with `indent-according-to-mode'

Every other value means don't apply any snippet-side indendation
after expansion (the manual per-line \"$>\" indentation still
applies)."
  :type '(choice (const :tag "Nothing"  nothing)
                 (const :tag "Fixed"    fixed)
                 (const :tag "Auto"     auto))
  :group 'yasnippet)

(defcustom yas/snippet-revival t
  "Non-nil means re-activate snippet fields after an when undoing
an exit from an active snippet or redoing a snippet expansion"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-key "TAB"
  "The key to bind as a trigger of snippet when `yas/minor-mode'
is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'. "
  :type 'string
  :group 'yasnippet)

(defcustom yas/next-field-key "TAB"
  "The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'. "
  :type 'string
  :group 'yasnippet)

(defcustom yas/prev-field-key "S-TAB"
  "The key to navigate to previous field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'. "
  :type 'string
  :group 'yasnippet)

(defcustom yas/clear-field-key "C-d"
  "The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'. "
  :type 'string
  :group 'yasnippet)

(defcustom yas/triggers-in-field t
  "If non-nil, allow `yas/next-field-key' to trigger a stacked
  snippet expansion.

Otherwise, `yas/next-field-key' just tries to move on to the next field"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/fallback-behavior 'call-other-command
  "How to act when `yas/trigger-key' does *not* expand a snippet.

The fall back behavior of YASnippet when it can't find a snippet
to expand.

`call-other-command' means try to temporarily disable
    YASnippet and call other command bound to `yas/trigger-key'.

`return-nil' means return do nothing."
  :type '(choice (const :tag "Call previous command"  'call-other-command)
                 (const :tag "Do nothing"    'return-nil))
  :group 'yasnippet)

(defcustom yas/choose-keys-first t
  "If non-nil, `yas/choose-snippet' prompts for key, then for template.

Otherwise `yas/choose-snippet' prompts for all possible
templates and inserts the selected one."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/use-menu t
  "Display a YASnippet menu in the menu bar.

If this is set to `t', all snippet template of the current
mode will be listed under the menu \"yasnippet\"."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger."
  :type 'string
  :group 'yasnippet)

(defcustom yas/show-all-modes-in-menu nil
  "Display \"artificial\" major modes in menu bar as well.

Currently, YASnippet only all \"real modes\" to menubar. For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'. There's really
no such mode like \"cc-mode\". So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes. The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/wrap-around-region t
  "If non-nil, snippet expansion wraps around selected region.

The wrapping occurs just before the snippet's exit marker. This
can be overriden on a per-snippet basis."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/good-grace nil
  "If non-nil, don't raise errors in inline elisp evaluation.

An error string \"[yas] error\" is returned instead."


  :type 'boolean
  :group 'yasnippet)

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight the currently active field of a snippet"
  :group 'yasnippet)

(defface yas/field-debug-face
  '()
  "The face used for debugging some overlays normally hidden"
  :group 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User semi-customizable variables
;;

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")

(eval-when-compile
  (define-key yas/keymap (read-kbd-macro yas/next-field-key) 'yas/next-field-or-maybe-expand)
  (define-key yas/keymap (read-kbd-macro yas/prev-field-key) 'yas/prev-field)
  (define-key yas/keymap (read-kbd-macro yas/clear-field-key) 'yas/clear-field-or-delete-char))

(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.

The hooks will be run in an environment where some variables bound to
proper values:

`yas/snippet-beg' : The beginning of the region of the snippet.

`yas/snippet-end' : Similar to beg.

Attention: These hooks are not run when exiting nested/stackd snippet expansion!")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run just before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (not (bobp))
            (or (equal 'font-lock-comment-face
                       (get-char-property (1- (point))
                                          'face))
                (equal 'font-lock-string-face
                       (get-char-property (1- (point))
                                          'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Condition to yasnippet local to each buffer.

The default value helps filtering out potential snippet
expansions inside comments and string literals, unless the
snippet itself contains a condition that returns the symbol
`force-in-comment'.

    * If yas/buffer-local-condition evaluate to nil, snippet
      won't be expanded.

    * If it evaluate to the a cons cell where the car is the
      symbol `require-snippet-condition' and the cdr is a
      symbol (let's call it \"requirement\"):
       * If the snippet has no condition, then it won't be
         expanded.
       * If the snippet has a condition but it evaluates to nil or
         error occured during evaluation, it won't be expanded.
       * If the snippet has a condition that evaluate to
         non-nil (let's call it \"result\"):
          * If \"requirement\" is t, the snippet is ready to be
            expanded.
          * If \"requirement\" is eq to \"result\", the snippet is ready
            to be expanded.
          * Otherwise the snippet won't be expanded.

    * If it evaluates to `always', snippet is unconditionally
      expanded.

    * If it evaluates to other non-nil value:
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
(make-variable-buffer-local 'yas/buffer-local-condition)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;; 

(defvar yas/version "0.5.6-nested-placeholders")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")

(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major-mode.")

(defvar yas/menu-keymap (make-sparse-keymap "YASnippet"))
;; empty menu will cause problems, so we insert some items

(eval-when-compile
  (define-key yas/menu-keymap [yas/about]
    '(menu-item "About" yas/about))

  (define-key yas/menu-keymap [yas/reload]
    '(menu-item "Reload all snippets" yas/reload-all))

  (define-key yas/menu-keymap [yas/load]
    '(menu-item "Load snippets..." yas/load-directory))

  (define-key yas/menu-keymap [yas/separator]
    '(menu-item "--")))

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")

(defvar yas/escaped-characters
  '(?\\ ?` ?' ?$ ?} )
  "A list of characters which *might* need to be escaped in
snippet templates")

(defconst yas/field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field")

(defconst yas/multi-dollar-lisp-expression-regexp
  "$\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression")

(defconst yas/backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"`(...)`\" expression")

(defconst yas/transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform")

(defconst yas/simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror")

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet minor mode
;;

(defvar yas/minor-mode-map (make-sparse-keymap)
  "The keymap of yas/minor-mode")

(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'yasnippet
  (define-key yas/minor-mode-map (read-kbd-macro yas/trigger-key) 'yas/expand))

(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode."
  (interactive)
  (yas/minor-mode 1))

(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))

(define-globalized-minor-mode yas/global-mode yas/minor-mode yas/minor-mode-on
  :group 'yasnippet) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal structs for template management
;; 

(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition)))
  "A template for a snippet."
  content
  name
  condition)

(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (parent nil))

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
  "Filter the templates using the applicable condition.

TEMPLATES is a list of cons (KEY . TEMPLATE) where KEY is a
string and TEMPLATE is a `yas/template' structure.

This function implements the rules described in
`yas/buffer-local-condition'. See that variables documentation."
  (let ((requirement (yas/require-template-specific-condition-p)))
    (if (eq requirement 'always)
	templates
      (remove-if-not #'(lambda (pair)
			 (let* ((condition (yas/template-condition (cdr pair)))
				(result (and condition
					     (yas/template-condition-predicate condition))))
			   (cond ((eq requirement t)
				  result)
				 (t
				  (eq requirement result))))) 
		     templates))))

(defun yas/snippet-table-fetch (table key)
  "Fetch a snippet binding to KEY from TABLE. If not found,
fetch from parent if any."
  (let* ((unfiltered (gethash key (yas/snippet-table-hash table)))
	 (templates  (yas/filter-templates-by-condition unfiltered)))
    (when (and (null templates)
               (not (null (yas/snippet-table-parent table))))
      (setq templates (yas/snippet-table-fetch
                       (yas/snippet-table-parent table)
                       key)))
    templates))

(defun yas/snippet-table-all-templates (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
		   (setq acc (append acc templates)))
	       (yas/snippet-table-hash table))
      (append (yas/filter-templates-by-condition acc)
	      (yas/snippet-table-all-templates (yas/snippet-table-parent table))))))

(defun yas/snippet-table-all-keys (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
		   (when (yas/filter-templates-by-condition templates)
		     (push key acc)))
	       (yas/snippet-table-hash table))
      (append acc
	      (yas/snippet-table-all-keys (yas/snippet-table-parent table))))))

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
;; 

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
  ;; TODO: This is a possible optimization point, the expression could
  ;; be stored in cons format instead of string,
  "Evaluate STRING and convert the result to string."
  (let ((retval (catch 'yas/exception
		  (condition-case err
		      (save-excursion
			(save-restriction
			  (save-match-data
			    (widen)
			    (let ((result (eval (read string))))
			      (when result
				(format "%s" result))))))
		    (error (if yas/good-grace
			       "[yas] elisp error!"
			     (error (format "[yas] elisp error: %s"
					    (error-message-string err)))))))))
    (when (and (consp retval)
	       (eq 'yas/exception (car retval)))
      (error (cdr retval)))
    retval))

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
 * key
 * group

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let ((name file-name) template bound condition key group)
    (if (re-search-forward "^# --\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^#\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (read (match-string-no-properties 2))))
                 (when (string= "group" (match-string-no-properties 1))
                   (setq group (match-string-no-properties 2)))
                 (when (string= "key" (match-string-no-properties 1))
                   (setq key (match-string-no-properties 2)))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (list key template name condition group)))

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
      (let ((where (if mark-active
		       (cons (region-beginning) (region-end))
		     (cons (point) (point)))))
	(yas/expand-snippet (car where)
			    (cdr where)
			    template)))))

(defun yas/modify-alist (alist key value)
  "Modify ALIST to map KEY to VALUE. return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
        (cons (cons key value)
              alist)
      (setcdr pair value)
      alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popping up for keys and templates
;; 
(defun yas/prompt-for-template-content (templates)
  "Interactively choose a template's content from the list
TEMPLATES."
  (let ((template (some #'(lambda (fn)
			    (funcall fn "Choose a snippet: " templates #'(lambda (template)
									   (yas/template-name template))))
			yas/prompt-functions)))
    (when template
      (yas/template-content template))))

(defun yas/prompt-for-keys (keys)
  "Interactively choose a template key from the list KEYS."
  (if keys
      (some #'(lambda (fn)
		(funcall fn "Choose a snippet key: " keys))
	    yas/prompt-functions)
    (message "[yas] no expansions possible here!")))

(defun yas/x-prompt (prompt choices &optional display-fn)
  (when (and window-system choices)
    (let ((keymap (cons 'keymap
			(cons
			 prompt
			 (mapcar (lambda (choice)
				   (list choice
					 'menu-item
					 (if display-fn
					     (funcall display-fn choice)
					   choice)
					 t))
				 choices)))))
      (when (cdr keymap)
	(car (x-popup-menu (if (fboundp 'posn-at-point)
			       (let ((x-y (posn-x-y (posn-at-point (point)))))
				 (list (list (+ (car x-y) 10)
					     (+ (cdr x-y) 20))
				       (selected-window)))
			     t)
			   keymap))))))

(defun yas/ido-prompt (prompt choices &optional display-fn)
  (when (featurep 'ido)
    (let* ((formatted-choices (or (and display-fn
				       (mapcar display-fn choices))
				  choices))
	   (chosen (and choices
			(ido-completing-read prompt
					     formatted-choices
					     nil
					     'require-match
					     nil
					     nil))))
      (when chosen
	(nth (position chosen formatted-choices) choices)))))

(defun yas/dropdown-prompt (prompt choices &optional display-fn)
  (when (featurep 'dropdown-list)
    ))

(defun yas/completing-prompt (prompt choices &optional display-fn)
  )

(defun yas/no-prompt (prompt choices &optional display-fn)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading snippets from files
;; 
(defun yas/load-directory-1 (directory &optional parent)
  
  "Really do the job of loading snippets from a directory
hierarchy."
  (let ((mode-sym (intern (file-name-nondirectory directory)))
        (snippets nil))
    (with-temp-buffer
      (dolist (file (yas/directory-files directory t))
        (when (file-readable-p file)
          (insert-file-contents file nil nil nil t)
          (let* ((snip (yas/parse-template))
                 (key (or (car snip)
                          (file-name-nondirectory file)))
                 (snip (cdr snip)))
            (push (cons key snip) snippets)))))
    (yas/define-snippets mode-sym
                         snippets
                         parent)
    (dolist (subdir (yas/directory-files directory nil))
      (yas/load-directory-1 subdir mode-sym))))

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
    (setq code (concat "(yas/initialize-bundle)"
		       "\n;;;###autoload" ; break through so that won't
		       "(require 'yasnippet-bundle)"))) ; be treated as magic comment

  (let ((dirs (or (and (listp snippet-roots) snippet-roots)
                  (list snippet-roots)))
        (bundle-buffer nil))
    (with-temp-buffer
      (setq bundle-buffer (current-buffer))
      (insert ";;; yasnippet-bundle.el --- "
              "Yet another snippet extension (Auto compiled bundle)\n")
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\""
              "  (yas/initialize)\n")
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
                          (yas/quote-string (nth 1 snippet))
                          " "
                          (if (nth 2 snippet)
                              (yas/quote-string (nth 2 snippet))
                            "nil")
                          " "
                          (if (nth 3 snippet)
                              (format "'%s" (nth 3 snippet))
                            "nil")
                          " "
                          (if (nth 4 snippet)
                              (yas/quote-string (nth 4 snippet))
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

      (insert ")\n\n" code "\n")
      (insert "(provide '"
              (file-name-nondirectory
               (file-name-sans-extension
                yasnippet-bundle))
              ")\n")
      (insert ";;; "
              (file-name-nondirectory yasnippet-bundle)
              " ends here\n")
      (setq buffer-file-name yasnippet-bundle)
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User level functions
;;; 

(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>/joaotavora <joaotavora@gmail.com>")))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define snippets for MODE.  SNIPPETS is a list of
snippet definition, of the following form:

 (KEY TEMPLATE NAME CONDITION GROUP)

or the NAME, CONDITION or GROUP may be omitted.  The optional 3rd
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
             (name (or (nth 2 snippet) (file-name-extension full-key)))
             (condition (nth 3 snippet))
             (group (nth 4 snippet))
             (template (yas/make-template (nth 1 snippet)
                                          (or name key)
                                          condition)))
        (yas/snippet-table-store snippet-table
                                 full-key
                                 key
                                 template)
        (when yas/use-menu
          (let ((group-keymap keymap))
            (when (and (not (null group))
                       (not (string= "" group)))
              (dolist (subgroup (mapcar #'make-symbol
                                        (split-string group "\\.")))
                (let ((subgroup-keymap (lookup-key group-keymap 
                                                   (vector subgroup))))
                  (when (null subgroup-keymap)
                    (setq subgroup-keymap (make-sparse-keymap))
                    (define-key group-keymap (vector subgroup)
                      `(menu-item ,(symbol-name subgroup)
                                  ,subgroup-keymap)))
                  (setq group-keymap subgroup-keymap))))
            (define-key group-keymap (vector (make-symbol full-key))
              `(menu-item ,(yas/template-name template)
                          ,(yas/make-menu-binding (yas/template-content 
                                                   template))
                          :keys ,(concat key yas/trigger-symbol)))))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
         (yas/snippet-table mode))
        (yas/snippet-table parent))
  (when yas/use-menu
    (define-key (yas/menu-keymap-for-mode mode) (vector 'parent-mode)
      `(menu-item "parent mode"
                  ,(yas/menu-keymap-for-mode parent)))))

(defun yas/define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.
NAME is a description to this template.  Also update
the menu if `yas/use-menu' is `t'.  CONDITION is the
condition attached to this snippet.  If you attach a
condition to a snippet, then it will only be expanded
when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition group))))

(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (undo 1)
    nil))

(defun yas/require-template-specific-condition-p ()
  "Decides if this buffer requests/requires snippet-specific
conditions to filter out potential expansions."
  (if (eq 'always yas/buffer-local-condition)
      'always
    (let ((local-condition (yas/template-condition-predicate
			    yas/buffer-local-condition)))
      (and local-condition
	   (consp local-condition)
	   (eq 'require-snippet-condition (car local-condition))
	   (symbolp (cdr local-condition))
	   (cdr local-condition)))))

(defun yas/expand (&optional field)
  "Expand a snippet."
  (interactive)
  (multiple-value-bind (templates start end) (if field
						 (save-restriction
						   (narrow-to-region (yas/field-start field) (yas/field-end field))
						   (yas/current-key))
					       (yas/current-key))
    (if templates
	(let ((template-content (or (and (rest templates) ;; more than one
					 (yas/prompt-for-template-content (mapcar #'cdr templates)))
				    (yas/template-content (cdar templates)))))
	  (when template-content
	    (yas/expand-snippet start end template-content)))
      (if (eq yas/fallback-behavior 'return-nil)
	  nil				; return nil
	(let* ((yas/minor-mode nil)
	       (command (key-binding (read-kbd-macro yas/trigger-key))))
	  (when (commandp command)
	    (call-interactively command)))))))

(defun yas/choose-snippet (&optional no-condition)
  "Choose a snippet to expand, pop-up a list of choices according
to `yas/prompt-function'.

With prefix argument NO-CONDITION, bypass filtering of snippets
by condition."
  (interactive "P")
  (let* ((yas/buffer-local-condition (or (and no-condition
					      'always)
					 yas/buffer-local-condition))
	 (templates (mapcar #'cdr
			    (if yas/choose-keys-first
				(let ((key (yas/prompt-for-keys (yas/snippet-table-all-keys (yas/current-snippet-table)))))
				  (when key
				    (yas/snippet-table-fetch (yas/current-snippet-table) key)))
			      (yas/snippet-table-all-templates (yas/current-snippet-table)))))
	 (template-content (and templates
				(or (and (rest templates) ;; more than one template for same key
					 (yas/prompt-for-template-content templates))
				    (yas/template-content (car templates)))))
	 (where (if mark-active
		    (cons (region-beginning) (region-end))
		  (cons (point) (point)))))
    (when template-content
      (yas/expand-snippet (car where) (cdr where) template-content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User convenience functions, for using in snippet definitions
;;;

(defun yas/substr (str pattern &optional subexp)
  "Search PATTERN in STR and return SUBEXPth match.

If found, the content of subexp group SUBEXP (default 0) is
  returned, or else the original STR will be returned."
  (let ((grp (or subexp 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

(defun yas/choose-value (&rest possibilities)
  "Prompt for a string in the list POSSIBILITIES."
  (some #'(lambda (fn)
	    (funcall fn "Choose: " possibilities))
	yas/prompt-functions))

(defun yas/throw (text)
  (throw 'yas/exception (cons 'yas/exception text)))

(defun yas/verify-value (&rest possibilities)
  "Verify that the current field value is in POSSIBILITIES

Otherwise throw exception."
  (when (and yas/moving-away (notany #'(lambda (pos) (string= pos yas/text)) possibilities))
    (yas/throw (format "[yas] field only allows %s" possibilities))))

(defun yas/field-value (number)
  (let* ((snippet (car (yas/snippets-at-point)))
	 (field (and snippet
		     (yas/snippet-find-field snippet number))))
    (when field
      (yas/field-text-for-display field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippet expansion and field management

(defvar yas/active-field-overlay nil
  "Overlays the currently active field")

(defvar yas/field-protection-overlays nil
  "Two overlays protect the current actipve field ")

(defvar yas/deleted-text nil
  "The text deleted in the last snippet expansion.")

(defvar yas/selected-text nil
  "The selected region deleted before the last snippet
  expansion.")

(defvar yas/start-column nil
  "The column where the snippet expansion started.")

(make-variable-buffer-local 'yas/active-field-overlay)
(make-variable-buffer-local 'yas/field-protection-overlays)
(make-variable-buffer-local 'yas/deleted-text)

(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

..."
  (fields '())
  (exit nil)
  (id (yas/snippet-next-id) :read-only t)
  (control-overlay nil)
  active-field
  ;; stacked expansion: the `previous-active-field' slot saves the
  ;; active field where the child expansion took place
  previous-active-field)

(defstruct (yas/field (:constructor yas/make-field (number start end parent-field)))
  "A field."
  number
  start end
  parent-field
  (mirrors '())
  (next nil)
  (prev nil)
  (transform nil)
  (modified-p nil))

(defstruct (yas/mirror (:constructor yas/make-mirror (start end transform)))
  "A mirror."
  start end
  (transform nil))

(defun yas/apply-transform (field-or-mirror field)
  "Calculate the value of the field/mirror. If there's a transform
for this field, apply it. Otherwise, returned nil."
  (let* ((yas/text (yas/field-text-for-display field))
	 (text yas/text)
	 (yas/modified-p (yas/field-modified-p field))
	 (yas/moving-away nil)
	 (transform (if (yas/mirror-p field-or-mirror)  
			(yas/mirror-transform field-or-mirror)
		      (yas/field-transform field-or-mirror)))
	 (start-point (if (yas/mirror-p field-or-mirror)  
			  (yas/mirror-start field-or-mirror)
			(yas/field-start field-or-mirror)))
	 (transformed (and transform
			   (save-excursion
			     (goto-char start-point)
			     (yas/eval-string transform)))))
    transformed))

(defsubst yas/replace-all (from to)
  "Replace all occurance from FROM to TO."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun yas/snippet-find-field (snippet number) 
  (find-if #'(lambda (field)
	       (eq number (yas/field-number field)))
	   (yas/snippet-fields snippet)))

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

(defun yas/field-probably-deleted-p (field)
  "Guess if FIELD was deleted because of his parent-field" 
  (and (zerop (- (yas/field-start field) (yas/field-end field)))
       (yas/field-parent-field field)))

(defun yas/snippets-at-point (&optional all-snippets)
  "Return a sorted list of snippets at point, most recently
inserted first."
  (sort
   (remove nil (remove-duplicates (mapcar #'(lambda (ov)
					      (overlay-get ov 'yas/snippet))
					  (if all-snippets
					      (overlays-in (point-min) (point-max))
					    (overlays-at (point))))))
   #'(lambda (s1 s2)
       (<= (yas/snippet-id s2) (yas/snippet-id s1)))))

(defun yas/next-field-or-maybe-expand ()
  "Try to expand a snippet at a key before point, otherwise
delegate to `yas/next-field'."
  (interactive)
  (if yas/triggers-in-field
      (let ((yas/fallback-behavior 'return-nil)
	    (active-field (overlay-get yas/active-field-overlay 'yas/field)))
	(when active-field
	  (unless (yas/expand active-field))
	  (yas/next-field)))
    (yas/next-field)))

(defun yas/next-field (&optional arg)
  "Navigate to next field.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (first (yas/snippets-at-point)))
	 (active-field (overlay-get yas/active-field-overlay 'yas/field))
         (live-fields (remove-if #'yas/field-probably-deleted-p (yas/snippet-fields snippet)))
	 (active-field-pos (position active-field live-fields))
	 (target-pos (+ arg active-field-pos))
	 (target-field (nth target-pos live-fields)))
    ;; First check if we're moving out of a field with a transform
    ;; 
    (when (and active-field
	       (yas/field-transform active-field))
      (let* ((yas/moving-away t)
	     (yas/text (yas/field-text-for-display active-field))
	     (text yas/text)
	     (yas/modified-p (yas/field-modified-p active-field)))
	(yas/eval-string (yas/field-transform active-field))))
    ;; Now actually move...
    (cond ((>= target-pos (length live-fields))
           (yas/exit-snippet snippet))
          (target-field
           (yas/move-to-field snippet target-field))
          (t
           nil))))

(defun yas/move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas/field-start field))
  (setf (yas/snippet-active-field snippet) field)
  (yas/make-move-active-field-overlay snippet field)
  (yas/make-move-field-protection-overlays snippet field)
  (overlay-put yas/active-field-overlay 'yas/field field)
  (unless (yas/field-modified-p field)
    (if (yas/field-update-display field snippet)
	(let ((inhibit-modification-hooks t))
	  (yas/update-mirrors snippet))
      (setf (yas/field-modified-p field) nil))))

(defun yas/prev-field ()
  "Navigate to prev field.  If there's none, exit the snippet."
  (interactive)
  (yas/next-field -1))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and commit the snippet.  Cleaning
up the snippet does not delete it!"
  (interactive)
  (goto-char (if (yas/snippet-exit snippet)
		 (yas/snippet-exit snippet)
	       (overlay-end (yas/snippet-control-overlay snippet)))))

;;; Apropos markers-to-points:
;;;
;;; This was ground useful for performance
;;; reasons, so that an excessive number of live markers arent kept
;;; aroung in the `buffer-undo-list'. However, in `markers-to-points',
;;; the set-to-nil markers can't simply be discarded and replaced with
;;; fresh ones in `points-to-markers'. The original marker that was
;;; just set to nilhas to be reused.
;;;
;;; This shouldn't bring horrible problems with undo/redo, but it
;;; would be one of the the first thing I'd remove if I was debugging that...
;;;

(defun yas/markers-to-points (snippet)
  "Convert all markers in SNIPPET to a cons (POINT . MARKER)
where POINT is the original position of the marker and MARKER is
the original marker object with the position set to nil."
  (dolist (field (yas/snippet-fields snippet))
    (let ((start (marker-position (yas/field-start field)))
	  (end (marker-position (yas/field-end field))))
      (set-marker (yas/field-start field) nil)
      (set-marker (yas/field-end field) nil)
      (setf (yas/field-start field) (cons start (yas/field-start field)))
      (setf (yas/field-end field) (cons end (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (let ((start (marker-position (yas/mirror-start mirror)))
	    (end (marker-position (yas/mirror-end mirror))))
	(set-marker (yas/mirror-start mirror) nil)
	(set-marker (yas/mirror-end mirror) nil)
	(setf (yas/mirror-start mirror) (cons start (yas/mirror-start mirror)))
	(setf (yas/mirror-end mirror) (cons end (yas/mirror-end mirror))))))
  (when (yas/snippet-exit snippet)
    (let ((exit (marker-position (yas/snippet-exit snippet))))
      (set-marker (yas/snippet-exit snippet) nil)
      (setf (yas/snippet-exit snippet) (cons exit (yas/snippet-exit snippet))))))

(defun yas/points-to-markers (snippet)
  "Convert all cons (POINT . MARKER) in SNIPPET to markers. This
is done by setting MARKER to POINT with `set-marker'."
  (dolist (field (yas/snippet-fields snippet))
    (setf (yas/field-start field) (set-marker (cdr (yas/field-start field)) (car (yas/field-start field))))
    (setf (yas/field-end field) (set-marker (cdr (yas/field-end field)) (car (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (setf (yas/mirror-start mirror) (set-marker (cdr (yas/mirror-start mirror)) (car (yas/mirror-start mirror))))
      (setf (yas/mirror-end mirror) (set-marker (cdr (yas/mirror-end mirror)) (car (yas/mirror-end mirror))))))
  (when (yas/snippet-exit snippet)
    (setf (yas/snippet-exit snippet) (set-marker (cdr (yas/snippet-exit snippet)) (car (yas/snippet-exit snippet))))))

(defun yas/commit-snippet (snippet &optional no-hooks)
  "Commit SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text.

Return a buffer position where the point should be placed if
exiting the snippet.

NO-HOOKS means don't run the `yas/after-exit-snippet-hook' hooks."

  (let ((control-overlay (yas/snippet-control-overlay snippet))
	yas/snippet-beg
	yas/snippet-end)
    ;;
    ;; Save the end of the moribund snippet in case we need to revive it
    ;; its original expansion.
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas/snippet-beg (overlay-start control-overlay))
      (setq yas/snippet-end (overlay-end control-overlay))
      (delete-overlay control-overlay))

    (let ((inhibit-modification-hooks t))
      (when yas/active-field-overlay
	(delete-overlay yas/active-field-overlay))
      (when yas/field-protection-overlays
	(mapcar #'delete-overlay yas/field-protection-overlays)))

    ;; stacked expansion: if the original expansion took place from a
    ;; field, make sure we advance it here at least to
    ;; `yas/snippet-end'...
    ;;
    (let ((previous-field (yas/snippet-previous-active-field snippet)))
      (when previous-field
	(yas/advance-field-and-parents-maybe previous-field yas/snippet-end)))

    ;; Convert all markers to points, 
    ;;
    (yas/markers-to-points snippet)

    ;; Take care of snippet revival
    ;;
    (if yas/snippet-revival
	(push `(apply yas/snippet-revive ,yas/snippet-beg ,yas/snippet-end ,snippet)
	      buffer-undo-list)
      ;; Dismember the snippet... this is useful if we get called
      ;; again from `yas/take-care-of-redo'....
      (setf (yas/snippet-fields snippet) nil))
    
    ;; XXX: `yas/after-exit-snippet-hook' should be run with
    ;; `yas/snippet-beg' and `yas/snippet-end' bound. That might not
    ;; be the case if the main overlay had somehow already
    ;; disappeared, which sometimes happens when the snippet's messed
    ;; up...
    ;;
    (unless no-hooks (run-hooks 'yas/after-exit-snippet-hook)))
  
  (message "[yas] snippet exited."))

(defun yas/check-commit-snippet ()
  "Checks if point exited the currently active field of the
snippet, if so cleans up the whole snippet up."
  (let* ((snippets (yas/snippets-at-point 'all-snippets))
	 (snippets-left snippets))
    (dolist (snippet snippets)
      (let ((active-field (yas/snippet-active-field snippet))) 
	(cond ((not (and active-field (yas/field-contains-point-p active-field)))
	       (setq snippets-left (delete snippet snippets-left))
	       (yas/commit-snippet snippet snippets-left))
	      ((and active-field
		    (or (not yas/active-field-overlay)
			(not (overlay-buffer yas/active-field-overlay))))
	       ;;
	       ;; stacked expansion: this case is mainly for recent
	       ;; snippet exits that place us back int the field of
	       ;; another snippet
	       ;;
	       (save-excursion
		 (yas/move-to-field snippet active-field)
		 (yas/update-mirrors snippet)))
	      (t
	       nil))))
    (unless snippets-left
      (remove-hook 'post-command-hook 'yas/post-command-handler 'local)
      (remove-hook 'pre-command-hook 'yas/pre-command-handler 'local))))

(defun yas/field-contains-point-p (field &optional point)
  (let ((point (or point
		   (point))))
    (and (>= point (yas/field-start field))
	 (<= point (yas/field-end field)))))

(defun yas/pre-command-handler () )

(defun yas/post-command-handler ()
  "Handles various yasnippet conditions after each command."
  (cond (yas/protection-violation
	 (goto-char yas/protection-violation)
	 (setq yas/protection-violation nil))
	((eq 'undo this-command)
	 ;;
	 ;; After undo revival the correct field is sometimes not
	 ;; restored correctly, this condition handles that
	 ;;
	 (let* ((snippet (car (yas/snippets-at-point)))
		(target-field (and snippet
				   (find-if-not #'yas/field-probably-deleted-p
						(remove nil
							(cons (yas/snippet-active-field snippet)
							      (yas/snippet-fields snippet)))))))
	   (when target-field
	     (yas/move-to-field snippet target-field))))
	((not (yas/undo-in-progress))
	 ;; When not in an undo, check if we must commit the snippet (use exited it). 
	 (yas/check-commit-snippet))))

(defun yas/field-text-for-display (field)
  "Return the propertized display text for field FIELD.  "
  (buffer-substring (yas/field-start field) (yas/field-end field)))

(defun yas/undo-in-progress ()
  "True if some kind of undo is in progress"
  (or undo-in-progress
      (eq this-command 'undo)
      (eq this-command 'redo)))

(defun yas/make-control-overlay (snippet start end)
  "Creates the control overlay that surrounds the snippet and
holds the keymap."
  (let ((overlay (make-overlay start
                               end
                               nil
                               nil 
                               t)))
    (overlay-put overlay 'keymap yas/keymap)
    (overlay-put overlay 'yas/snippet snippet)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun yas/clear-field-or-delete-char (&optional field)
  "Clears an unmodified field if at field start, otherwise
deletes a character normally."
  (interactive)
  (let ((field (or field
		   (and yas/active-field-overlay
			(overlay-buffer yas/active-field-overlay)
			(overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
		(not (yas/field-modified-p field))
		(eq (point) (marker-position (yas/field-start field))))
	   (yas/clear-field field))
	  (t
	   (call-interactively 'delete-char)))))

(defun yas/clear-field (field)
  "Deletes the region of FIELD and sets it modified state to t" 
  (setf (yas/field-modified-p field) t)
  (delete-region (yas/field-start field) (yas/field-end field)))

(defun yas/advance-field-and-parents-maybe (field end)
  "Advance FIELDs end-marker to END and recurse for parent fields

This is needed since markers don't \"rear-advance\" like overlays"
  (when (< (yas/field-end field) end)
    (set-marker (yas/field-end field) end)
    (when (yas/field-parent-field field)
      (yas/advance-field-and-parents-maybe (yas/field-parent-field field) end))))

(defun yas/make-move-active-field-overlay (snippet field)
  "Place the active field overlay in SNIPPET's FIELD.

Move the overlay, or create it if it does not exit."
  (if (and yas/active-field-overlay
	   (overlay-buffer yas/active-field-overlay))
      (move-overlay yas/active-field-overlay
		    (yas/field-start field)
		    (yas/field-end field))
    (setq yas/active-field-overlay
	  (make-overlay (yas/field-start field)
			(yas/field-end field)
			nil nil t))
    (overlay-put yas/active-field-overlay 'face 'yas/field-highlight-face)
    ;;(overlay-put yas/active-field-overlay 'evaporate t)
    (overlay-put yas/active-field-overlay 'modification-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-in-front-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-behind-hooks '(yas/on-field-overlay-modification))))

(defun yas/on-field-overlay-modification (overlay after? beg end &optional length)
  "Clears the field and updates mirrors, conditionally.

Only clears the field if it hasn't been modified and it point it
at field start. This hook doesn't do anything if an undo is in
progress."
  (unless (yas/undo-in-progress)
    (let ((field (overlay-get yas/active-field-overlay 'yas/field)))
      (cond (after?
	     (yas/advance-field-and-parents-maybe field (overlay-end overlay))
	     (yas/field-update-display field (car (yas/snippets-at-point)))
	     (yas/update-mirrors (car (yas/snippets-at-point))))
	    (field
	     (when (and (not after?)
			(not (yas/field-modified-p field))
			(eq (point) (if (markerp (yas/field-start field))
					(marker-position (yas/field-start field))
				      (yas/field-start field))))
	       (yas/clear-field field))
	     (setf (yas/field-modified-p field) t))))))

;;; Apropos protection overlays:
;;;
;;; These exist for nasty users who will try to delete parts of the
;;; snippet outside the active field. Actual protection happens in
;;; `yas/on-protection-overlay-modification'.
;;;
;;; Currently this signals an error which inhibits the command. For
;;; commands that move point (like `kill-line'), point is restored in
;;; the `yas/post-command-handler' using a global
;;; `yas/protection-violation' variable.
;;;
;;; Alternatively, I've experimented with an implementation that
;;; commits the snippet before actually calling `this-command'
;;; interactively, and then signals an eror, which is ignored. but
;;; blocks all other million modification hooks. This presented some
;;; problems with stacked expansion.
;;;

(defun yas/make-move-field-protection-overlays (snippet field)
  "Place protection overlays surrounding SNIPPET's FIELD.

Move the overlays, or create them if they do not exit."
  (let ((start (yas/field-start field))
	(end (yas/field-end field)))
    ;; First check if the (1+ end) is contained in the buffer,
    ;; otherwise we'll have to do a bit of cheating and silently
    ;; insert a newline. the `(1+ (buffer-size))' should prevent this
    ;; when using stacked expansion
    ;; 
    (when (< (1+ (buffer-size)) (1+ end))
      (save-excursion
	(let ((inhibit-modification-hooks t))
	  (goto-char (point-max))
	  (newline))))
    ;; go on to normal overlay creation/moving
    ;; 
    (cond ((and yas/field-protection-overlays
		(every #'overlay-buffer yas/field-protection-overlays))
	   (move-overlay (first yas/field-protection-overlays) (1- start) start)
	   (move-overlay (second yas/field-protection-overlays) end (1+ end)))
	  (t
	   (setq yas/field-protection-overlays
		 (list (make-overlay (1- start) start nil t nil)
		       (make-overlay end (1+ end) nil t nil)))
	   (dolist (ov yas/field-protection-overlays)
	     (overlay-put ov 'face 'yas/field-debug-face)
	     (overlay-put ov 'yas/snippet snippet)
	     ;; (overlay-put ov 'evaporate t)
	     (overlay-put ov 'modification-hooks '(yas/on-protection-overlay-modification)))))))

(defvar yas/protection-violation nil
  "When non-nil, signals attempts to erronesly exit or modify the snippet.

Functions in the `post-command-hook', for example
`yas/post-command-handler' can check it and reset its value to nil. The variables value is the point where the violation originated")

(defun yas/on-protection-overlay-modification (overlay after? beg end &optional length)
  "Signals a snippet violation, then issues error.

The error should be ignored in `debug-ignored-errors'"
  (cond ((not (or after?
		  (yas/undo-in-progress)))
	 (setq yas/protection-violation (point))
	 (error "Exit the snippet first!"))))

(add-to-list 'debug-ignored-errors "^Exit the snippet first!$")

;;; Apropos stacked expansion:
;;;
;;; the parent snippet does not run its fields modification hooks
;;; (`yas/on-field-overlay-modification' and
;;; `yas/on-protection-overlay-modification') while the child snippet
;;; is active. This means, among other things, that the mirrors of the
;;; parent snippet are not updated, this only happening when one exits
;;; the child snippet.
;;;
;;; Unfortunately, this also puts some ugly (and not fully-tested)
;;; bits of code in `yas/expand-snippet' and
;;; `yas/commit-snippet'. I've tried to mark them with "stacked
;;; expansion:".
;;;
;;; This was thought to be safer in in an undo/redo perpective, but
;;; maybe the correct implementation is to make the globals
;;; `yas/active-field-overlay' and `yas/field-protection-overlays' be
;;; snippet-local and be active even while the child snippet is
;;; running. This would mean a lot of overlay modification hooks
;;; running, but if managed correctly (including overlay priorities)
;;; they should account for all situations...
;;;

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)
  (goto-char start)

  ;; stacked expansion: shoosh the overlay modification hooks
  ;; 
  (let ((key (buffer-substring-no-properties start end))
	(inhibit-modification-hooks t)
	(column (current-column))
	snippet)

    ;; Delete the trigger key, this *does* get undo-recorded.
    ;;
    (delete-region start end)
    
    ;; Narrow the region down to the template, shoosh the
    ;; `buffer-undo-list', and create the snippet, the new snippet
    ;; updates its mirrors once, so we are left with some plain text.
    ;; The undo action for deleting this plain text will get recorded
    ;; at the end of this function.
    (save-restriction
      (narrow-to-region start start)
      (condition-case err
	  (let ((buffer-undo-list t))
	    ;; snippet creation might evaluate users elisp, which
	    ;; might generate errors, so we have to be ready to catch
	    ;; them mostly to make the undo information
	    ;;
	    (setq yas/start-column (save-restriction (widen) (current-column)))
	    (insert template)
	    (setq yas/deleted-text key)
	    (setq yas/selected-text (when mark-active key))
	    (setq snippet (yas/snippet-create (point-min) (point-max))))
	(error
	 (push (cons (point-min) (point-max)) buffer-undo-list)
	 (error (format "[yas] parse error: %s" (cadr err))))))

    ;; stacked-expansion: This checks for stacked expansion, save the
    ;; `yas/previous-active-field' and advance its boudary.
    ;;
    (let ((existing-field (and yas/active-field-overlay
			       (overlay-buffer yas/active-field-overlay)
			       (overlay-get yas/active-field-overlay 'yas/field))))
      (when existing-field
	(setf (yas/snippet-previous-active-field snippet) existing-field)
	(yas/advance-field-and-parents-maybe existing-field (overlay-end yas/active-field-overlay))))
    
    ;; Move to the first of fields, or exit the snippet to its exit
    ;; point
    ;; 
    (let ((first-field (car (yas/snippet-fields snippet))))
      (cond (first-field
	     (yas/move-to-field snippet first-field))
	    (t
	     (yas/exit-snippet snippet))))
    ;; Push two undo actions: the deletion of the inserted contents of
    ;; the new snippet (whitout the "key") followed by an apply of
    ;; `yas/take-care-of-redo' on the newly inserted snippet boundaries
    ;; 
    (let ((start (overlay-start (yas/snippet-control-overlay snippet)))
	  (end (overlay-end (yas/snippet-control-overlay snippet))))
      (push (cons start end) buffer-undo-list)
      (push `(apply yas/take-care-of-redo ,start ,end ,snippet)
	    buffer-undo-list)))
  (message "[yas] snippet expanded."))

(defun yas/take-care-of-redo (beg end snippet)
  "Commits SNIPPET, which in turn pushes an undo action for
reviving it.

Meant to exit in the `buffer-undo-list'."
  (yas/commit-snippet snippet 'no-hooks))

(defun yas/snippet-revive (beg end snippet)
  "Revives the SNIPPET and creates a control overlay from BEG to
END.

BEG and END are, we hope, the original snippets boudaries. All
the markers/points exiting existing inside SNIPPET should point
to their correct locations *at the time the snippet is revived*.

After revival, push the `yas/take-care-of-redo' in the
`buffer-undo-list'"
  ;; Reconvert all the points to markers
  ;;
  (yas/points-to-markers snippet)
  ;; When at least one editable field existed in the zombie snippet,
  ;; try to revive the whole thing...
  ;;
  (let ((target-field (or (yas/snippet-active-field snippet)
			  (car (yas/snippet-fields snippet)))))
    (when target-field
      (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet beg end))
      (overlay-put (yas/snippet-control-overlay snippet) 'yas/snippet snippet)
   
      (yas/move-to-field snippet target-field)

      (add-hook 'post-command-hook 'yas/post-command-handler nil t)
      (add-hook 'pre-command-hook 'yas/pre-command-handler t t)
  
      (push `(apply yas/take-care-of-redo ,beg ,end ,snippet)
	    buffer-undo-list))))

(defun yas/snippet-create (begin end)
  "Creates a snippet from an template inserted between BEGIN and END.

Returns the newly created snippet." 
  (let ((snippet (yas/make-snippet)))
    (goto-char begin)
    (yas/snippet-parse-create snippet)

    ;; Sort and link each field
    (yas/snippet-sort-link-fields snippet)
    
    ;; Update the mirrors for the first time
    (yas/update-mirrors snippet)

    ;; Create keymap overlay for snippet
    (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet (point-min) (point-max)))

    ;; Move to end
    (goto-char (point-max))

    ;; Setup hooks
    (add-hook 'post-command-hook 'yas/post-command-handler nil t)
    (add-hook 'pre-command-hook 'yas/pre-command-handler t t)
    
    snippet))

(defun yas/snippet-sort-link-fields (snippet)
  (setf (yas/snippet-fields snippet)
	(sort (yas/snippet-fields snippet)
	      '(lambda (field1 field2)
		 (yas/snippet-field-compare field1 field2))))
  (let ((prev nil))
    (dolist (field (yas/snippet-fields snippet))
      (setf (yas/field-prev field) prev)
      (when prev
	(setf (yas/field-next prev) field))
      (setq prev field))))

(defun yas/snippet-parse-create (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes"
  (let ((parse-start (point)))
    ;; protect quote and backquote escapes
    ;; 
    (yas/protect-escapes '(?` ?'))
    ;; replace all backquoted expressions
    ;;
    (goto-char parse-start)
    (yas/replace-backquotes)
    ;; protect escapes again since previous stepds might have
    ;; generated more characters needing escapinge
    ;;
    (goto-char parse-start)
    (yas/protect-escapes)
    ;; parse fields
    ;; 
    (goto-char parse-start)
    (yas/field-parse-create snippet)
    ;; parse simple mirrors
    ;; 
    (goto-char parse-start)
    (yas/simple-mirror-parse-create snippet)
    ;; parse mirror transforms
    ;;
    (goto-char parse-start)
    (yas/transform-mirror-parse-create snippet)
    ;; restore escapes
    ;;
    (goto-char parse-start)
    (yas/restore-escapes)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    (yas/indent snippet)))

(defun yas/indent (snippet)
  (save-excursion
  (cond ((eq yas/indent-line 'fixed)
	 (let* ((indent (if indent-tabs-mode
			    (concat (make-string (/ column tab-width) ?\t)
				    (make-string (% column tab-width) ?\ ))
			  (make-string (current-colum) ?\ ))))
	   (goto-char (point-min))
	   (while (and (zerop (forward-line))
		       (= (current-column) 0))
	     (insert indent))))
	((eq yas/indent-line 'auto)
	 (let ((end (set-marker (make-marker) (point-max))))
	   (save-restriction
	     (widen)
	     ;; XXX: Here seems to be the indent problem:
	     ;;
	     ;; `indent-according-to-mode' uses whatever
	     ;; `indent-line-function' is available. Some
	     ;; implementations of these functions delete text
	     ;; before they insert. If there happens to be a marker
	     ;; just after the text being deleted, the insertion
	     ;; actually happens after the marker, which misplaces
	     ;; it.
	     ;;
	     ;; This would also happen if we had used overlays with
	     ;; the `front-advance' property set to nil.
	     ;;
	     (while (and (zerop (forward-line))
			 (<= (point) end))
	       (goto-char (yas/real-line-beginning))
	       (insert-before-markers "Y")
	       (indent-according-to-mode)
	       (backward-delete-char 1))
	     (set-marker end nil))))
	(t
	 nil)))
  (save-excursion
    (while (re-search-forward "$>" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (when (not (eq yas/indent-line 'auto))        
	(indent-according-to-mode)))))

(defun yas/real-line-beginning ()
  (let ((c (char-after (line-beginning-position)))
        (n (line-beginning-position)))
    (while (or (eql c ?\ )
               (eql c ?\t))
      (incf n)
      (setq c (char-after n)))
    n))


(defun yas/escape-string (escaped)
  (concat "YASESCAPE" (format "%d" escaped) "PROTECTGUARD"))

(defun yas/protect-escapes (&optional escaped)
  "Protect all escaped characters with their numeric ASCII value."
  (mapc #'(lambda (escaped)
	    (yas/replace-all (concat "\\" (char-to-string escaped))
			     (yas/escape-string escaped)))
  (or escaped yas/escaped-characters)))

(defun yas/restore-escapes ()
  "Restore all escaped characters from their numeric ASCII value."
  (mapc #'(lambda (escaped)
	    (yas/replace-all (yas/escape-string escaped)
			     (char-to-string escaped)))
  yas/escaped-characters))

(defun yas/replace-backquotes ()
  "Replace all the \"`(lisp-expression)`\"-style expression
  with their evaluated value"
  (while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
  (let ((transformed (yas/eval-string (match-string 1))))
    (goto-char (match-end 0))
    (insert transformed)
    (delete-region (match-beginning 0) (match-end 0)))))

(defun yas/scan-sexps (from count)
  (condition-case err
      (with-syntax-table (standard-syntax-table)
	(scan-sexps from count))
    (error
     nil)))

(defun yas/field-parse-create (snippet &optional parent-field)
  "Parse most field expression, except for the simple one \"$n\".

The following count as a field:

* \"${n: text}\", for a numbered field with default text, as long as N is not 0;
* \"${n: text$(expression)}, the same with a lisp expression;
* the same as above but unnumbered, (no N:) and number is calculated automatically.

When multiple expressions are found, only the last one counts."
  (save-excursion
  (while (re-search-forward yas/field-regexp nil t)
    (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
	   (number (and (match-string-no-properties 1)
			(string-to-number (match-string-no-properties 1))))
	   (brand-new-field (and real-match-end-0
				 (not (save-match-data
					(eq (string-match "$[ \t\n]+(" (match-string-no-properties 2)) 0)))
				 (not (and number (zerop number)))
				 (yas/make-field number
						 (set-marker (make-marker) (match-beginning 2))
						 (set-marker (make-marker) (1- real-match-end-0))
						 parent-field))))
      (when brand-new-field
	(delete-region (1- real-match-end-0) real-match-end-0)
	(delete-region (match-beginning 0) (match-beginning 2))
	(push brand-new-field (yas/snippet-fields snippet))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (yas/field-start brand-new-field) (yas/field-end brand-new-field))
	    (goto-char (point-min))
	    (yas/field-parse-create snippet brand-new-field)))))))
  (when parent-field
  (save-excursion
    (while (re-search-forward yas/multi-dollar-lisp-expression-regexp nil t)
      (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1)))
	(when real-match-end-0
	  (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1) real-match-end-0))) 
	    (setf (yas/field-transform parent-field) lisp-expression-string))
	  (delete-region (match-beginning 0) real-match-end-0)))))))

(defun yas/transform-mirror-parse-create (snippet)
  "Parse the \"${n:$(lisp-expression)}\" mirror transformations."
  (while (re-search-forward yas/transform-mirror-regexp nil t)
  (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
	 (number (string-to-number (match-string-no-properties 1)))
	 (field (and number
		     (not (zerop number))
		     (yas/snippet-find-field snippet number))))
    (when (and real-match-end-0
	       field) 
      (push (yas/make-mirror (set-marker (make-marker) (match-beginning 0))
			     (set-marker (make-marker) (match-beginning 0))
			     (buffer-substring-no-properties (match-beginning 2)
							     (1- real-match-end-0)))
	    (yas/field-mirrors field))
      (delete-region (match-beginning 0) real-match-end-0)))))

(defun yas/simple-mirror-parse-create (snippet)
  "Parse the simple \"$n\" mirrors and the exit-marker."
  (while (re-search-forward yas/simple-mirror-regexp nil t)
  (let ((number (string-to-number (match-string-no-properties 1))))
    (cond ((zerop number)
	     
	   (setf (yas/snippet-exit snippet)
		 (set-marker (make-marker) (match-end 0)))
	   (save-excursion
	     (goto-char (match-beginning 0))
	     (when (and yas/wrap-around-region yas/selected-text)
	       (insert yas/selected-text))
	     (delete-region (point) (yas/snippet-exit snippet))))
	  (t
	   (let ((field (yas/snippet-find-field snippet number)))
	     (if field
		 (push (yas/make-mirror (set-marker (make-marker) (match-beginning 0))
					(set-marker (make-marker) (match-beginning 0))
					nil)
		       (yas/field-mirrors field))
	       (push (yas/make-field number
				     (set-marker (make-marker) (match-beginning 0))
				     (set-marker (make-marker) (match-beginning 0))
				     nil)
		     (yas/snippet-fields snippet))))
	   (delete-region (match-beginning 0) (match-end 0)))))))

(defun yas/update-mirrors (snippet)
  "Updates all the mirrors of SNIPPET."
  (save-excursion
  (dolist (field (yas/snippet-fields snippet))
    (dolist (mirror (yas/field-mirrors field))
      ;; stacked expansion: I added an `inhibit-modification-hooks'
      ;; here, for safety, may need to remove if we the mechanism is
      ;; altered.
      ;; 
      (let ((inhibit-modification-hooks t))
	(yas/mirror-update-display mirror field))))))

(defun yas/mirror-update-display (mirror field)
  "Update MIRROR according to FIELD (and mirror transform)."
  (let ((reflection (or (yas/apply-transform mirror field)
			(yas/field-text-for-display field))))
  (when (and reflection
	     (not (string= reflection (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror)))))
    (goto-char (yas/mirror-start mirror))
    (insert reflection)
    (if (> (yas/mirror-end mirror) (point))
	(delete-region (point) (yas/mirror-end mirror))
      (set-marker (yas/mirror-end mirror) (point))))))

(defun yas/field-update-display (field snippet)
  "Much like `yas/mirror-update-display', but for fields"
  (when (yas/field-transform field)
  (let ((inhibit-modification-hooks t)
	(transformed (yas/apply-transform field field))
	(point (point)))
    (when (and transformed
	       (not (string= transformed (buffer-substring-no-properties (yas/field-start field) (yas/field-end field)))))
      (setf (yas/field-modified-p field) t)
      (goto-char (yas/field-start field))
      (insert transformed)
      (if (> (yas/field-end field) (point))
	  (delete-region (point) (yas/field-end field))
	(set-marker (yas/field-end field) (point)))
      t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode stuff
;;
(defvar yas/font-lock-keywords
  (append '(("^#.*$" . font-lock-comment-face))
  lisp-font-lock-keywords
  lisp-font-lock-keywords-1
  lisp-font-lock-keywords-2
  '(("$\\([0-9]+\\)"
     (0 font-lock-keyword-face)
     (1 font-lock-string-face t))
    ("${\\([0-9]+\\):?"
     (0 font-lock-keyword-face)
     (1 font-lock-warning-face t))
    ("\\(\\$\\)(" 1 font-lock-preprocessor-face)
    ("}"
     (0 font-lock-keyword-face)))))

(define-derived-mode yas/snippet-editing-mode fundamental-mode "YASnippet"
  "A mode for editing yasnippets"
  (setq font-lock-defaults '(yas/font-lock-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug functions.  Use (or change) at will whenever needed.
;;

(defun yas/debug-some-vars ()
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
  (princ "Interesting YASnippet vars: \n\n")

  (princ (format "\nPost command hook: %s\n" post-command-hook))
  (princ (format "\nPre  command hook: %s\n" pre-command-hook))

  (princ (format "%s live snippets in total" (length (yas/snippets-at-point (quote all-snippets)))))
  (princ (format "%s live snippets at point:" (length (yas/snippets-at-point))))
    
  (dolist (snippet (yas/snippets-at-point))
    (princ (format "\tid: %s and active field from %s to %s covering \"%s\"\n"
		   (yas/snippet-id snippet)
		   (marker-position (yas/field-start (yas/snippet-active-field snippet)))
		   (marker-position (yas/field-end (yas/snippet-active-field snippet)))
		   (buffer-substring-no-properties (yas/field-start (yas/snippet-active-field snippet)) (yas/field-end (yas/snippet-active-field snippet))))))

  (princ (format "\nUndo is %s and point-max is %s.\n"
		 (if (eq buffer-undo-list t)
		     "DISABLED"
		   "ENABLED")
		 (point-max)))
  (unless (eq buffer-undo-list t)
    (princ (format "Undpolist has %s elements. First 10 elements follow:\n" (length buffer-undo-list)))
    (let ((first-ten (subseq buffer-undo-list 0 19)))
      (dolist (undo-elem first-ten)
	(princ (format "%2s:  %s\n" (position undo-elem first-ten) (truncate-string-to-width (format "%s" undo-elem) 70))))))))

(defun yas/exterminate-package ()
  (interactive)
  (yas/minor-mode -1)
  (unintern 'yasnippet)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(defun yas/debug-test (&optional quiet)
  (interactive "P")
  (yas/load-directory "~/Source/yasnippet/snippets/")
  ;;(kill-buffer (get-buffer "*YAS TEST*"))
  (set-buffer (switch-to-buffer "*YAS TEST*"))
  (mapcar #'yas/commit-snippet (yas/snippets-at-point 'all-snippets))
  (erase-buffer)
  (setq buffer-undo-list nil)
  (c-mode)
  (yas/initialize)
  (yas/minor-mode 1)
  (let ((abbrev))
    ;; (if (require 'ido nil t)
    ;; 	(setq abbrev (ido-completing-read "Snippet abbrev: " '("crazy" "prip" "prop")))
    ;;   (setq abbrev "prop"))
  (setq abbrev "bosta")
  (insert abbrev))
  (when quiet
  (add-hook 'post-command-hook 'yas/debug-some-vars 't 'local))
  )

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

;; disable c-electric-* serial command in YAS fields
(add-hook 'c-mode-common-hook
          '(lambda ()
	  (make-variable-buffer-local 'yas/keymap)
	  (dolist (k '(":" ">" ";" "<" "{" "}"))
	    (define-key yas/keymap
	      k 'self-insert-command))))


;;; yasnippet.el ends here
