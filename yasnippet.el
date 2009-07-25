;;; Yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid

;; Authors: pluskid <pluskid@gmail.com>, joaotavora <joaotavora@gmail.com>
;; Version: 0.6.0
;; Package-version: 0.6.0b
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
;; 
;;   1. In your .emacs file:
;;	  (add-to-list 'load-path "/dir/to/yasnippet.el")
;;        (require 'yasnippet)
;;   2. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   3. In your .emacs file
;;        (setq yas/root-directory "~/.emacs/snippets")
;;        (yas/load-directory yas/root-directory)
;;   4. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   5. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 4. and 5. are optional, you don't have to use the minor
;;   mode to use YASnippet.
;;
;;
;;   Major commands are:
;;
;;       M-x yas/expand
;;
;;           Try to expand snippets before point.  In `yas/minor-mode',
;;           this is bound to `yas/trigger-key' which you can customize.
;;
;;       M-x yas/load-directory
;;
;;           Prompts you for a directory hierarchy of snippets to load.
;;
;;       M-x yas/insert-snippet
;;
;;	     Prompts you for possible snippet expansion if that is
;;	     possible according to buffer-local and snippet-local
;;	     expansion conditions.  With prefix argument, ignore these
;;	     conditions.
;;
;;       M-x yas/find-snippets
;;
;;           Lets you find the snippet file in the directory the
;;           snippet was loaded from (if it exists) like
;;           `find-file-other-window'.
;;
;;       M-x yas/visit-snippet-file
;;
;;           Prompts you for possible snippet expansions like
;;           `yas/insert-snippet', but instead of expanding it, takes
;;           you directly to the snippet definition's file, if it
;;           exists.
;;
;;       M-x yas/load-snippet-buffer
;;
;;           When editing a snippet, this loads the snippet.  This is
;;           bound to "C-c C-c" while in the `snippet-mode' editing
;;           mode.
;;
;;       M-x yas/tryout-snippet
;;
;;	     When editing a snippet, this opens a new empty buffer,
;;	     sets it to the appropriate major mode and inserts the
;;	     snippet there, so you can see what it looks like.  This is
;;	     bound to "C-c C-t" while in `snippet-mode'.
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
;;   
;;        M-x customize-group RET yasnippet RET
;;
;;   For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(require 'cl)
(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;

(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :group 'editing)

(defcustom yas/root-directory nil
  "Root directory that stores the snippets for each major mode.

Can also be a list of strings, for multiple root directories."
  :type '(string)
  :group 'yasnippet)

(defcustom yas/prompt-functions '(yas/x-prompt
				  yas/dropdown-prompt
				  yas/completing-prompt
				  yas/ido-prompt
				  yas/no-prompt)
  "Functions to prompt for keys, templates, etc interactively."
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
  "Non-nil means re-activate snippet fields after undo/redo."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-key "TAB"
  "The key bound to `yas/expand' when function `yas/minor-mode' is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/next-field-key "TAB"
  "The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/prev-field-key '("<backtab>" "<S-tab>")
  "The key to navigate to previous field when a snippet is active.

Can also be a list of keys.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/skip-and-clear-key "C-d"
  "The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/triggers-in-field nil
  "If non-nil, `yas/next-field-key' can trigger stacked expansions.

Otherwise, `yas/next-field-key' just tries to move on to the next
field"
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
  "If non-nil, `yas/insert-snippet' prompts for key, then for template.

Otherwise `yas/insert-snippet' prompts for all possible
templates and inserts the selected one."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/use-menu t
  "Display a YASnippet menu in the menu bar.

If this is set to t, all snippet template of the current
mode will be listed under the menu \"yasnippet\"."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger."
  :type 'string
  :group 'yasnippet)

(defcustom yas/show-all-modes-in-menu nil
  "Display \"artificial\" major modes in menu bar as well.

Currently, YASnippet only all \"real modes\" to menubar.  For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'.  There's really
no such mode like \"cc-mode\".  So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes.  The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/wrap-around-region nil
  "If non-nil, snippet expansion wraps around selected region.

The wrapping occurs just before the snippet's exit marker.  This
can be overriden on a per-snippet basis."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/good-grace t
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
  "The keymap active while a snippet expansion is in progress.")

(defun yas/define-some-keys (keys keymap definition)
  "Bind KEYS to DEFINITION in KEYMAP, read with `read-kbd-macro'."
  (let ((keys (or (and (listp keys) keys)
		  (list keys))))
    (dolist (key keys)
      (define-key keymap (read-kbd-macro key) definition))))

(eval-when-compile
  (yas/define-some-keys yas/next-field-key yas/keymap 'yas/next-field-or-maybe-expand)
  (yas/define-some-keys yas/prev-field-key yas/keymap 'yas/prev-field)
  (yas/define-some-keys yas/skip-and-clear-key yas/keymap 'yas/skip-and-clear-or-delete-char))

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

(defvar yas/version "0.6.0b")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major mode.")

(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major mode.")

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")

(defvar yas/escaped-characters
  '(?\\ ?` ?' ?$ ?} )
  "List of characters which *might* need to be escaped.")

(defconst yas/field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field.")

(defconst yas/multi-dollar-lisp-expression-regexp
  "$+[ \t\n]*\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression.")

(defconst yas/backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"`lisp-expression`\" expression." )

(defconst yas/transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform.")

(defconst yas/simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror.")

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode stuff
;;
;; TODO: XXX: This is somehow needed in Carbon Emacs for MacOSX 
(defvar last-buffer-undo-list nil)

(defvar yas/minor-mode-map (make-sparse-keymap)
  "The keymap used when function `yas/minor-mode' is active.")

(defun yas/init-keymap-and-menu ()
  (easy-menu-define yas/minor-mode-menu
    yas/minor-mode-map
    "Menu used when YAS/minor-mode is active."
    (cons "YASnippet"
	  (mapcar #'(lambda (ent)
		      (when (third ent)
			(define-key yas/minor-mode-map (third ent) (second ent)))
		      (vector (first ent) (second ent) t))
		  (list (list "--")
			(list "Expand trigger" 'yas/expand (read-kbd-macro yas/trigger-key))
			(list "Insert at point..." 'yas/insert-snippet "\C-c&\C-s")
			(list "Visit snippet file..." 'yas/visit-snippet-file "\C-c&\C-v")
			(list "Find snippets..." 'yas/find-snippets "\C-c&\C-f")
			(list "About" 'yas/about)
			(list "Reload-all-snippets" 'yas/reload-all)
			(list "Load snippets..." 'yas/load-directory))))))

(eval-when-compile
  (yas/init-keymap-and-menu))

(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}"
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'yasnippet)

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
	    ("${" font-lock-keyword-face)
	    ("$[0-9]+?" font-lock-preprocessor-face)
	    ("\\(\\$(\\)" 1 font-lock-preprocessor-face)
	    ("}"
	     (0 font-lock-keyword-face)))))

(defvar snippet-mode-map (make-sparse-keymap))
(define-key snippet-mode-map "\C-c\C-c" 'yas/load-snippet-buffer)
(define-key snippet-mode-map "\C-c\C-t" 'yas/tryout-snippet)


(define-derived-mode snippet-mode text-mode "YASnippet"
  "A mode for editing yasnippets"
  (setq font-lock-defaults '(yas/font-lock-keywords))
  (set (make-local-variable 'require-final-newline) nil)
  (use-local-map snippet-mode-map))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal structs for template management
;; 

(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition env file)))
  "A template for a snippet."
  content
  name
  condition
  env
  file)

(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (default-directory nil)
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
`yas/buffer-local-condition'.  See that variables documentation."
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
  (when table
    (let* ((unfiltered (gethash key (yas/snippet-table-hash table)))
	   (templates  (yas/filter-templates-by-condition unfiltered)))
      (when (and (null templates)
		 (not (null (yas/snippet-table-parent table))))
	(setq templates (yas/snippet-table-fetch
			 (yas/snippet-table-parent table)
			 key)))
      templates)))

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
  "Store a snippet template in the TABLE."
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

(defun yas/snippet-table-get-create (mode &optional directory)
  "Get the snippet table corresponding to MODE.

Optional DIRECTORY gets recorded as the default directory to
search for snippet files if the retrieved/created table didn't
already have such a property."
  (let ((table (gethash mode
			yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table))
      (puthash mode table yas/snippet-tables))
    (unless (or (not directory) (yas/snippet-table-default-directory table))
      (setf (yas/snippet-table-default-directory table)
	    directory))
    table))

(defun yas/current-snippet-table (&optional mode-symbol dont-search-parents)
  "Get the snippet table for current major-mode."
  (let ((mode (or mode-symbol
		  major-mode)))
    (or (gethash mode
		 yas/snippet-tables)
	(and (not dont-search-parents)
	     (get mode 'derived-mode-parent)
	     (yas/current-snippet-table (get mode 'derived-mode-parent))))))

(defun yas/menu-keymap-for-mode (mode)
  "Get the menu keymap correspondong to MODE."
  (let ((keymap (gethash mode yas/menu-table)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (puthash mode
	       keymap yas/menu-table))
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

(defun yas/parse-template (&optional file)
  "Parse the template in the current buffer.

Optional FILE is the absolute file name of the file being
parsed.

Return a snippet-definition, i.e. a list

 (KEY TEMPLATE NAME CONDITION GROUP ENV)

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
 * env

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let* ((name (and file (file-name-nondirectory file)))
	 (key name)
	 template
	 bound
	 condition
	 group
	 env)
    (if (re-search-forward "^# --\n" nil t)
	(progn (setq template
		     (buffer-substring-no-properties (point)
						     (point-max)))
	       (setq bound (point))
	       (goto-char (point-min))
	       (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
		 (when (string= "name" (match-string-no-properties 1))
		   (setq name (match-string-no-properties 2)))
		 (when (string= "condition" (match-string-no-properties 1))
		   (setq condition (read (match-string-no-properties 2))))
		 (when (string= "group" (match-string-no-properties 1))
		   (setq group (match-string-no-properties 2)))
		 (when (string= "env" (match-string-no-properties 1))
		   (setq env (match-string-no-properties 2)))
		 (when (string= "key" (match-string-no-properties 1))
		   (setq key (match-string-no-properties 2)))))
      (setq template
	    (buffer-substring-no-properties (point-min) (point-max))))
    (list key template name condition group env file)))

(defun yas/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
		   (string-match "~$"
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  `(lambda () (interactive) (yas/expand-from-menu ,template)))

(defun yas/expand-from-menu (template)
  (let ((where (if mark-active
		   (cons (region-beginning) (region-end))
		 (cons (point) (point)))))
    (yas/expand-snippet (car where)
			(cdr where)
			(yas/template-content template))))

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
(defun yas/prompt-for-template (templates &optional prompt)
  "Interactively choose a template from the list TEMPLATES.

TEMPLATES is a list of `yas/template'."
  (let ((template (some #'(lambda (fn)
			    (funcall fn (or prompt "Choose a snippet: ")
				     templates #'(lambda (template)
						   (yas/template-name template))))
			yas/prompt-functions)))
    template))

(defun yas/prompt-for-keys (keys &optional prompt)
  "Interactively choose a template key from the list KEYS."
  (if keys
      (some #'(lambda (fn)
		(funcall fn (or prompt "Choose a snippet key: ") keys))
	    yas/prompt-functions)))

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
  (when (and (featurep 'ido)
	     ido-mode)
    (let* ((formatted-choices (or (and display-fn
				       (mapcar display-fn choices))
				  choices))
	   (chosen (and formatted-choices
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
    (let* ((formatted-choices (or (and display-fn
				       (mapcar display-fn choices))
				  choices))
	   (chosen (and formatted-choices
			(nth (dropdown-list formatted-choices)
			     choices))))
      chosen)))

(defun yas/completing-prompt (prompt choices &optional display-fn)
  (let* ((formatted-choices (or (and display-fn
				     (mapcar display-fn choices))
				choices))
	 (chosen (and formatted-choices
		      (completing-read prompt
				       formatted-choices
				       nil
				       'require-match
				       nil
				       nil))))
    (when chosen
      (nth (position chosen formatted-choices) choices))))

(defun yas/no-prompt (prompt choices &optional display-fn)
  (first choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading snippets from files
;; 
(defun yas/load-directory-1 (directory &optional parent)
  
  "Recursively load snippet templates from DIRECTORY."

  (let ((mode-sym (intern (file-name-nondirectory directory)))
        (snippet-defs nil))
    (with-temp-buffer
      (dolist (file (yas/subdirs directory 'no-subdirs-just-files))
        (when (file-readable-p file)
          (insert-file-contents file nil nil nil t)
          (push (yas/parse-template file)
		snippet-defs))))
    (yas/define-snippets mode-sym
                         snippet-defs
                         parent
			 directory)
    (dolist (subdir (yas/subdirs directory))
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
  (dolist (dir (yas/subdirs directory))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "done.")))

(defun yas/reload-all ()
  "Reload all snippets and rebuild the YASnippet menu. "

  (interactive)
  (let ((restore-global-mode nil)
	(restore-minor-mode nil))
    (setq yas/snippet-tables (make-hash-table))
    (setq yas/menu-table (make-hash-table))
    (setf (cdr yas/minor-mode-menu) nil)
    (setf (cdr yas/minor-mode-map) nil)
    (when yas/global-mode
      (yas/global-mode -1)
      (setq restore-global-mode t))

    (when yas/minor-mode
      (yas/minor-mode -1)
      (setq restore-minor-mode t))

    (yas/init-keymap-and-menu)

    (if yas/root-directory
	(if (listp yas/root-directory)
	    (dolist (directory yas/root-directory)
	      (yas/load-directory directory))
	  (yas/load-directory yas/root-directory))
      (call-interactively 'yas/load-directory))


    (when restore-minor-mode
      (yas/minor-mode 1))

    (when restore-global-mode
      (yas/global-mode 1))

    (message "done.")))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnipept Bundle

(defun yas/initialize ()
  "For backward compatibility, enable `yas/minor-mode' globally"
  (yas/global-mode 1))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code dropdown)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.
SNIPPET-ROOTS is a list of root directories that contains the
snippets definition. YASNIPPET is the yasnippet.el file
path. YASNIPPET-BUNDLE is the output file of the compile
result. CODE is the code you would like to used to initialize
yasnippet. Last optional argument DROPDOWN is the filename of the
dropdown-list.el library.

Here's the default value for all the parameters:

 (yas/compile-bundle \"yasnippet.el\"
                     \"./yasnippet-bundle.el\"
                     '(\"snippets\")
                     \"(yas/initialize)\")

..

"
  (when (null yasnippet)
    (setq yasnippet "yasnippet.el"))
  (when (null yasnippet-bundle)
    (setq yasnippet-bundle "./yasnippet-bundle.el"))
  (when (null snippet-roots)
    (setq snippet-roots '("snippets")))
  (when (null dropdown)
    (setq dropdown "dropdown-list.el"))
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
      (insert "\n")
      (when dropdown
	(insert-file-contents dropdown))
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\""
              "  (yas/global-mode 1)\n")
      (flet ((yas/define-snippets
              (mode snippets &optional parent directory)
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
			;; (if directory
                        ;;     (concat "\"" directory "\"")
                        ;;   "nil")
                        ")\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/subdirs dir))
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

(defun yas/define-snippets (mode snippets &optional parent-mode directory)
  "Define snippets for MODE.  SNIPPETS is a list of
snippet definitions, each taking the following form:

 (KEY TEMPLATE NAME CONDITION GROUP)

NAME, CONDITION or GROUP may be omitted.  Optional PARENT-MODE
can be used to specify the parent mode of MODE.  That is, when
looking a snippet in MODE failed, it can refer to its parent
mode.  The PARENT-MODE does not need to be a real mode.

Optional DIRECTORY is recorded in the `yas/snippet-table' if it
is created for the first time. Then, it becomes the default
directory to find snippet files.


"
  (let ((snippet-table (yas/snippet-table-get-create mode directory))
        (parent-table (if parent-mode
                          (yas/snippet-table-get-create parent-mode)
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
      (define-key yas/minor-mode-menu (vector mode)
        `(menu-item ,(symbol-name mode) ,keymap)))
    (dolist (snippet snippets)
      (let* ((full-key (car snippet))
             (key (file-name-sans-extension full-key))
             (name (or (nth 2 snippet) (file-name-extension full-key)))
             (condition (nth 3 snippet))
             (group (nth 4 snippet))
	     (template (yas/make-template (nth 1 snippet)
                                          (or name key)
                                          condition
					  (nth 5 snippet)
					  (nth 6 snippet))))
        (yas/snippet-table-store snippet-table
                                 full-key
                                 key
                                 template)
        (when yas/use-menu
          (let ((group-keymap keymap))
	    ;; delete this entry from another group if already exists
	    ;; in some other group. An entry is considered as existing
	    ;; in another group if its name string-matches.
	    (yas/delete-from-keymap group-keymap name)
  
	    ;; ... then add this entry to the correct group
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
                          ,(yas/make-menu-binding template)
                          :keys ,(concat key yas/trigger-symbol)))))))))

(defun yas/delete-from-keymap (keymap name)
  "Recursively delete items name NAME from KEYMAP and its submenus.

Skip any submenus named \"parent mode\""
  ;; First of all, r ecursively enter submenus, i.e. the tree is
  ;; searched depth first so that stale submenus can be found in the
  ;; higher passes.
  ;; 
  (mapc #'(lambda (item)
	    (when (and (keymapp (fourth item))
		       (stringp (third item))
		       (not (string= (third item)
				     "parent mode")))
	      (yas/delete-from-keymap (fourth item) name)))
	(rest keymap))
  ;;
  (when (keymapp keymap)
    (let ((pos-in-keymap))
      (while (setq pos-in-keymap (position-if #'(lambda (item)
						  (and (listp item)
						       (or
							;; the menu item we want to delete
							(and (eq 'menu-item (second item))
							     (third item)
							     (and (string= (third item) name)))
							;; a stale subgroup
							(and (keymapp (fourth item))
							     (null (rest (fourth item)))))))
					      keymap))
	(setf (nthcdr pos-in-keymap keymap)
	      (nthcdr (+ 1 pos-in-keymap) keymap))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
         (yas/snippet-table-get-create mode))
        (yas/snippet-table-get-create parent))
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
	(let ((template (or (and (rest templates) ;; more than one
				 (yas/prompt-for-template (mapcar #'cdr templates)))
			    (cdar templates))))
	  (when template
	    (yas/expand-snippet start
				end
				(yas/template-content template)
				(yas/template-env template))))
      (if (eq yas/fallback-behavior 'return-nil)
	  nil				; return nil
	(let* ((yas/minor-mode nil)
	       (command (key-binding (read-kbd-macro yas/trigger-key))))
	  (when (commandp command)
	    (call-interactively command)))))))

(defun yas/insert-snippet (&optional no-condition)
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
	 (template (and templates
			(or (and (rest templates) ;; more than one template for same key
				 (yas/prompt-for-template templates))
			    (car templates))))
	 (where (if mark-active
		    (cons (region-beginning) (region-end))
		  (cons (point) (point)))))
    (if template
	(yas/expand-snippet (car where)
			    (cdr where)
			    (yas/template-content template)
			    (yas/template-env template))
      (message "[yas] No snippets can be inserted here!"))))

(defun yas/visit-snippet-file ()
  "Choose a snippet to edit, selection like `yas/insert-snippet'.

Only success if selected snippet was loaded from a file.  Put the
visited file in `snippet-mode'."
  (interactive)
  (let* ((yas/buffer-local-condition 'always)
	 (templates (mapcar #'cdr
			    (if yas/choose-keys-first
				(let ((key (yas/prompt-for-keys (yas/snippet-table-all-keys (yas/current-snippet-table))
								"Choose a snippet key to edit: ")))
				  (when key
				    (yas/snippet-table-fetch (yas/current-snippet-table) key)))
			      (yas/snippet-table-all-templates (yas/current-snippet-table)))))
	 (template (and templates
			(or (and (rest templates) ;; more than one template for same key
				 (yas/prompt-for-template templates
							  "Choose a snippet template to edit: "))
			    (car templates)))))

    (when template
      (let ((file (yas/template-file template)))
	(cond ((and file (file-exists-p file))
	       (find-file-other-window file)
	       (snippet-mode))
	      (file
	       (message "Original file %s no longer exists!" file))
	      (t
	       (message "This snippet was not loaded from a file!")))))))

(defun yas/guess-snippet-directory ()
  "Try to guess the suitable yassnippet based on `major-mode'"
  (let ((loaded-root (or (and (listp yas/root-directory)
			      (first yas/root-directory))
			 yas/root-directory))
	(mode major-mode)
	(path))
    (when loaded-root
      (while mode
	(setq path (format "%s/%s"
			   mode
			   (or path
			       "")))
	(setq mode (get mode 'derived-mode-parent)))
      (concat loaded-root
	      (unless (string-match "/$" loaded-root) "/")
	      path))))


(defun yas/find-snippets (&optional same-window)
  "Looks for snippets file in the current mode's directory.

This can be used to create new snippets for the currently active
major mode."
  (interactive "P")
  (let* ((current-table (yas/current-snippet-table major-mode 'dont-search-parents))
	 (parents-table (yas/current-snippet-table major-mode))
	 (parents-directory (and parents-table
				 (yas/snippet-table-default-directory parents-table)))
	 (guessed-directory (or (and current-table
				     (yas/snippet-table-default-directory current-table))
				(yas/guess-snippet-directory)
				default-directory))
	 (buffer))
    (unless (file-exists-p guessed-directory)
      (if (y-or-n-p (format "Guessed directory (%s) does not exist! Create? " guessed-directory))
	  (make-directory guessed-directory 'also-make-parents)
	(if parents-directory
	    (setq guessed-directory parents-directory)
	  (setq guessed-directory default-directory))))
    (let ((default-directory guessed-directory))
      (setq buffer (call-interactively (if same-window
					   'find-file
					 'find-file-other-window)))
      (when buffer
	(save-excursion
	  (set-buffer buffer)
	  (when (eq major-mode 'fundamental-mode)
	    (snippet-mode)))))))


(defun yas/compute-major-mode-and-parent (file &optional prompt-if-failed)
  (let* ((file-dir (and file
			(directory-file-name (file-name-directory file))))
	 (major-mode-name (and file-dir
			       (file-name-nondirectory file-dir)))
	 (parent-file-dir (and file-dir
			       (directory-file-name (file-name-directory file-dir))))
	 (parent-mode-name (and parent-file-dir
				(file-name-nondirectory parent-file-dir)))
	 (major-mode-sym (or (and major-mode-name
				  (intern major-mode-name))
			     (when prompt-if-failed
			       (read-from-minibuffer "[yas] Cannot auto-detect major mode! Enter a major mode: "))))
	 (parent-mode-sym (and parent-mode-name
			       (intern parent-mode-name))))
    (if (fboundp major-mode-sym)
	(cons major-mode-sym
	      (when (fboundp parent-mode-sym)
		parent-mode-sym)))))

(defun yas/load-snippet-buffer (&optional kill)
  "Parse and load current buffer's snippet definition.

With optional prefix argument KILL quit the window and buffer."
  (interactive "P")
  (if buffer-file-name
      (let ((major-mode-and-parent (yas/compute-major-mode-and-parent buffer-file-name)))
	(if major-mode-and-parent
	    (let* ((parsed (yas/parse-template buffer-file-name))
		   (name (and parsed
			      (third parsed))))
	      (when name
		(yas/define-snippets (car major-mode-and-parent)
				     (list parsed)
				     (cdr major-mode-and-parent))
		(when (and (buffer-modified-p)
			   (y-or-n-p "Save snippet? "))
		  (save-buffer))
		(if kill
		    (quit-window kill)
		  (message "[yas] Snippet \"%s\" loaded for %s." name (car major-mode-and-parent)))))
	  (message "[yas] Cannot load snippet for unknown major mode")))
    (message "Save the buffer as a file first!")))

(defun yas/tryout-snippet (&optional debug)
  "Test current buffers's snippet template in other buffer."
  (interactive "P")
  (let* ((major-mode-and-parent (yas/compute-major-mode-and-parent buffer-file-name))
	 (parsed (and major-mode-and-parent
		      (fboundp (car major-mode-and-parent))
		      (yas/parse-template (symbol-name (car major-mode-and-parent)))))
	 (template (and parsed
			(yas/make-template (second parsed) (third parsed) nil (sixth parsed) nil))))
    (cond (template
	   (let ((buffer-name (format "*YAS TEST: %s*" (yas/template-name template))))
	     (set-buffer (switch-to-buffer buffer-name))
	     (erase-buffer)
	     (setq buffer-undo-list nil)
	     (funcall (car major-mode-and-parent))
	     (yas/expand-snippet (point-min) (point-max) (yas/template-content template) (yas/template-env template))
	     (when debug
	       (add-hook 'post-command-hook 'yas/debug-some-vars 't 'local))))
	  (t
	   (message "[yas] Coulnd not parse template!")))))

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

(defun yas/choose-value (possibilities)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
	      yas/modified-p)
    (some #'(lambda (fn)
	      (funcall fn "Choose: " possibilities))
	  yas/prompt-functions)))

(defun yas/key-to-value (alist)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
	      yas/modified-p)
    (let ((key (read-key-sequence "")))
      (when (stringp key)
	(or (cdr (find key alist :key #'car :test #'string=))
	    key)))))

(defun yas/throw (text)
  "Throw a yas/exception with TEXT as the reason."
  (throw 'yas/exception (cons 'yas/exception text)))

(defun yas/verify-value (possibilities)
  "Verify that the current field value is in POSSIBILITIES

Otherwise throw exception."
  (when (and yas/moving-away-p (notany #'(lambda (pos) (string= pos yas/text)) possibilities))
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
  "Overlays the currently active field.")

(defvar yas/field-protection-overlays nil
  "Two overlays protect the current active field ")

(defvar yas/deleted-text nil
  "The text deleted in the last snippet expansion.")

(defvar yas/selected-text nil
  "The selected region deleted on the last snippet expansion.")

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
  previous-active-field
  force-exit)

(defstruct (yas/field (:constructor yas/make-field (number start end parent-field)))
  "A field."
  number
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  (back-adjacent-fields nil)
  (back-adjacent-mirrors nil))

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
	 (yas/moving-away-p nil)
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

(defsubst yas/replace-all (from to &optional text)
  "Replace all occurance from FROM to TO.

With optional string TEXT do it in that string."
  (goto-char (point-min))
  (if text
      (replace-regexp-in-string from to text t t)
    (while (search-forward from nil t)
      (replace-match to t t text))))

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
	  (unless (yas/expand active-field)
	    (yas/next-field))))
    (yas/next-field)))

(defun yas/next-field (&optional arg)
  "Navigate to next field.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (first (yas/snippets-at-point)))
	 (active-field (overlay-get yas/active-field-overlay 'yas/field))
         (live-fields (remove-if #'(lambda (field)
				     (and (not (eq field active-field))
					  (yas/field-probably-deleted-p field)))
				 (yas/snippet-fields snippet)))
	 (active-field-pos (position active-field live-fields))
	 (target-pos (and active-field-pos (+ arg active-field-pos)))
	 (target-field (nth target-pos live-fields)))
    ;; First check if we're moving out of a field with a transform
    ;; 
    (when (and active-field
	       (yas/field-transform active-field))
      (let* ((yas/moving-away-p t)
	     (yas/text (yas/field-text-for-display active-field))
	     (text yas/text)
	     (yas/modified-p (yas/field-modified-p active-field)))
	;;; primary field transform: exit call to field-transform
	(yas/eval-string (yas/field-transform active-field))))
    ;; Now actually move...
    (cond ((>= target-pos (length live-fields))
           (yas/exit-snippet snippet))
          (target-field
           (yas/move-to-field snippet target-field))
          (t
           nil))))

(defun yas/place-overlays (snippet field)
  "Correctly place overlays for SNIPPET's FIELD"
  (yas/make-move-field-protection-overlays snippet field)
  (yas/make-move-active-field-overlay snippet field))

(defun yas/move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas/field-start field))
  (setf (yas/snippet-active-field snippet) field)
  (yas/place-overlays snippet field)
  (overlay-put yas/active-field-overlay 'yas/field field)
  ;;; primary field transform: first call to snippet transform
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
  "Goto exit-marker of SNIPPET."
  (interactive)
  (setf (yas/snippet-force-exit snippet) t)
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
      (when (and yas/snippet-end previous-field)
	(yas/advance-field-end-marker previous-field yas/snippet-end)))

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
	(cond ((or (prog1 (yas/snippet-force-exit snippet)
		     (setf (yas/snippet-force-exit snippet) nil))
		   (not (and active-field (yas/field-contains-point-p active-field))))
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

(defun yas/skip-and-clear-or-delete-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-char'."
  (interactive)
  (let ((field (or field
		   (and yas/active-field-overlay
			(overlay-buffer yas/active-field-overlay)
			(overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
		(not (yas/field-modified-p field))
		(eq (point) (marker-position (yas/field-start field))))
	   (yas/skip-and-clear field)
	   (yas/next-field 1))
	  (t
	   (call-interactively 'delete-char)))))

(defun yas/skip-and-clear (field)
  "Deletes the region of FIELD and sets it modified state to t"
  (setf (yas/field-modified-p field) t)
  (delete-region (yas/field-start field) (yas/field-end field)))

(defun yas/advance-field-end-marker (field newend)
  "Advance FIELDs end-marker to NEWEND and recurse for parent fields"
  (when (< (yas/field-end field) newend)
    (set-marker (yas/field-end field) newend)
    (when (yas/field-parent-field field)
      (yas/advance-field-end-marker (yas/field-parent-field field) newend)))
  ;; take care of adjacent fields
  (let ((adjacents (yas/field-back-adjacent-fields field)))
    (when adjacents
      (dolist (adjacent adjacents)
	(when (< (yas/field-start adjacent) newend)
	  (set-marker (yas/field-start adjacent) newend))
	(yas/advance-field-end-marker adjacent newend))))
  ;; take care of adjacent mirrors
  (let ((adjacents (yas/field-back-adjacent-mirrors field)))
    (when adjacents
      (dolist (adjacent adjacents)
	(when (< (yas/mirror-start adjacent) newend)
	  (set-marker (yas/mirror-start adjacent) newend))))))

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
    (overlay-put yas/active-field-overlay 'yas/snippet snippet)
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
	     (yas/advance-field-end-marker field (overlay-end overlay))
	     ;;; primary field transform: normal calls to expression
	     (let ((saved-point (point)))
	       (yas/field-update-display field (car (yas/snippets-at-point)))
	       (goto-char saved-point))
	     (yas/update-mirrors (car (yas/snippets-at-point))))
	    (field
	     (when (and (not after?)
			(not (yas/field-modified-p field))
			(eq (point) (if (markerp (yas/field-start field))
					(marker-position (yas/field-start field))
				      (yas/field-start field))))
	       (yas/skip-and-clear field))
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
    (when (< (buffer-size) end)
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

(defun yas/expand-snippet (start end template &optional snippet-vars)
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
	    (setq snippet
		  (if snippet-vars
		      (eval `(let ,(read snippet-vars)
			       (yas/snippet-create (point-min) (point-max))))
		    (yas/snippet-create (point-min) (point-max)))))
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
	(yas/advance-field-end-marker existing-field (overlay-end yas/active-field-overlay))))
    
    ;; Exit the snippet immediately if no fields
    ;;
    (unless (yas/snippet-fields snippet)
      (yas/exit-snippet snippet))
    
    ;; Push two undo actions: the deletion of the inserted contents of
    ;; the new snippet (whitout the "key") followed by an apply of
    ;; `yas/take-care-of-redo' on the newly inserted snippet boundaries
    ;; 
    (let ((start (overlay-start (yas/snippet-control-overlay snippet)))
	  (end (overlay-end (yas/snippet-control-overlay snippet))))
      (push (cons start end) buffer-undo-list)
      (push `(apply yas/take-care-of-redo ,start ,end ,snippet)
	    buffer-undo-list))
    ;; Now, move to the first field
    ;;
    (let ((first-field (car (yas/snippet-fields snippet))))
      (when first-field
	(yas/move-to-field snippet first-field))))
  (message "[yas] snippet expanded."))

(defun yas/take-care-of-redo (beg end snippet)
  "Commits SNIPPET, which in turn pushes an undo action for
reviving it.

Meant to exit in the `buffer-undo-list'."
  ;; slightly optimize: this action is only needed for snippets with
  ;; at least one field
  (when (yas/snippet-fields snippet)
    (yas/commit-snippet snippet 'no-hooks)))

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

    ;; Calculate field and mirror adjacencies
    (yas/calculate-adjacencies snippet)
    
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
		 (yas/snippet-field-compare field1 field2)))))

(defun yas/calculate-adjacencies (snippet)
  ;; For each field in the snippet
  ;; 
  (dolist (field (yas/snippet-fields snippet))
    ;; Calculate its adjacencies to other mirrors and fields
    ;; 
    (dolist (otherfield (yas/snippet-fields snippet))
      (dolist (mirror (yas/field-mirrors otherfield))
	(when (= (yas/field-end field) (yas/mirror-start mirror))
	  (push mirror (yas/field-back-adjacent-mirrors field))))
      (when (and (not (eq otherfield field))
		 (= (yas/field-end field) (yas/field-start otherfield)))
	(when (not (find field (yas/field-back-adjacent-fields otherfield)))
	  (push otherfield (yas/field-back-adjacent-fields field)))))
    ;; Calculate the adjacencies of each one of its mirrors
    ;;
    ;; TODO: Known bug.
    ))

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
    ;; parse fields with {}
    ;; 
    (goto-char parse-start)
    (yas/field-parse-create snippet)
    ;; parse simple mirrors and fields
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
    ;; update mirrors for the first time
    ;;
    (yas/update-mirrors snippet)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    (yas/indent snippet)))

(defun yas/indent (snippet)
  (save-excursion
    (while (re-search-forward "$>" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (when (not (eq yas/indent-line 'auto))
	(indent-according-to-mode))))
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
	   (let ((end (set-marker (make-marker) (point-max)))
		 (snippet-markers (yas/collect-snippet-markers snippet)))
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
	       (while (and (zerop (forward-line 1))
			   (not (eobp))
			   (<= (point) end))
		 (goto-char (yas/real-line-beginning))
		 (let ((trouble-markers (remove-if-not #'(lambda (marker)
							   (= marker (point)))
						       snippet-markers)))
		       (indent-according-to-mode)
		       (mapc #'(lambda (marker)
				 (set-marker marker (point)))
			     trouble-markers)
		   (indent-according-to-mode)))
	       (set-marker end nil))))
	  (t
	   nil))))

(defun yas/collect-snippet-markers (snippet)
  "Make a list of all the markers used by SNIPPET."
  (let (markers)
    (dolist (field (yas/snippet-fields snippet))
      (push (yas/field-start field) markers)
      (push (yas/field-end field) markers)
      (dolist (mirror (yas/field-mirrors field))
	(push (yas/mirror-start mirror) markers)
	(push (yas/mirror-end mirror) markers)))
    (when (and (yas/snippet-exit snippet)
	       (marker-buffer (yas/snippet-exit snippet)))
      (push (yas/snippet-exit snippet) markers))
    markers))

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

(defun yas/restore-escapes (&optional text)
  "Restore all escaped characters from their numeric ASCII value.

With optional string TEXT do it in string instead"
  (let ((changed-text text)
	(text-provided-p text))
    (mapc #'(lambda (escaped)
	      (setq changed-text
		    (yas/replace-all (yas/escape-string escaped)
				     (char-to-string escaped)
				     (when text-provided-p changed-text))))
	  yas/escaped-characters)
    changed-text))

(defun yas/replace-backquotes ()
  "Replace all the \"`(lisp-expression)`\"-style expression
  with their evaluated value"
  (while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
  (let ((transformed (yas/eval-string (yas/restore-escapes (match-string 1)))))
    (goto-char (match-end 0))
    (when transformed (insert transformed))
    (delete-region (match-beginning 0) (match-end 0)))))

(defun yas/scan-sexps (from count)
  (condition-case err
      (with-syntax-table (standard-syntax-table)
	(scan-sexps from count))
    (error
     nil)))

(defun yas/make-marker (pos)
  "Create a marker at POS with `nil' `marker-insertion-type'"
  (let ((marker (set-marker (make-marker) pos)))
    (set-marker-insertion-type marker nil)
    marker))

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
					(eq (string-match "$[ \t\n]*(" (match-string-no-properties 2)) 0)))
				 (not (and number (zerop number)))
				 (yas/make-field number
						 (yas/make-marker (match-beginning 2))
						 (yas/make-marker (1- real-match-end-0))
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
      (let* ((real-match-end-1 (yas/scan-sexps (match-beginning 1) 1)))
	(when real-match-end-1
	  (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1) real-match-end-1)))
	    (setf (yas/field-transform parent-field) (yas/restore-escapes lisp-expression-string)))
	  (delete-region (match-beginning 0) real-match-end-1)))))))

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
      (push (yas/make-mirror (yas/make-marker (match-beginning 0))
			     (yas/make-marker (match-beginning 0))
			     (yas/restore-escapes (buffer-substring-no-properties (match-beginning 2)
										  (1- real-match-end-0))))
	    (yas/field-mirrors field))
      (delete-region (match-beginning 0) real-match-end-0)))))

(defun yas/simple-mirror-parse-create (snippet)
  "Parse the simple \"$n\" mirrors and the exit-marker."
  (while (re-search-forward yas/simple-mirror-regexp nil t)
  (let ((number (string-to-number (match-string-no-properties 1))))
    (cond ((zerop number)
	     
	   (setf (yas/snippet-exit snippet)
		 (yas/make-marker (match-end 0)))
	   (save-excursion
	     (goto-char (match-beginning 0))
	     (when (and yas/wrap-around-region yas/selected-text)
	       (insert yas/selected-text))
	     (delete-region (point) (yas/snippet-exit snippet))))
	  (t
	   (let ((field (yas/snippet-find-field snippet number)))
	     (if field
		 (push (yas/make-mirror (yas/make-marker (match-beginning 0))
					(yas/make-marker (match-beginning 0))
					nil)
		       (yas/field-mirrors field))
	       (push (yas/make-field number
				     (yas/make-marker (match-beginning 0))
				     (yas/make-marker (match-beginning 0))
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
	(yas/mirror-update-display mirror field)
	;; Take care of the fields adjacent to this mirror's back
	;; TODO: Known bug
	
	;; `yas/place-overlays' is needed if the active field and
	;; protected overlays have been changed because of insertions
	;; in `yas/mirror-update-display'
	;;
	(when (eq field (yas/snippet-active-field snippet))
	  (yas/place-overlays snippet field)))))))

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
;; Debug functions.  Use (or change) at will whenever needed.
;;

(defun yas/debug-some-vars ()
  "Debug snippets, fields, mirrors and the `buffer-undo-list'."
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
  (princ "Interesting YASnippet vars: \n\n")

  (princ (format "\nPost command hook: %s\n" post-command-hook))
  (princ (format "\nPre  command hook: %s\n" pre-command-hook))

  (princ (format "%s live snippets in total\n" (length (yas/snippets-at-point (quote all-snippets)))))
  (princ (format "%s live snippets at point:\n\n" (length (yas/snippets-at-point))))
    
  (dolist (snippet (yas/snippets-at-point))
    (princ (format "\tsid: %s active field %d from %s to %s covering \"%s\"\n"
		   (yas/snippet-id snippet)
		   (yas/field-number (yas/snippet-active-field snippet))
		   (marker-position (yas/field-start (yas/snippet-active-field snippet)))
		   (marker-position (yas/field-end (yas/snippet-active-field snippet)))
		   (buffer-substring-no-properties (yas/field-start (yas/snippet-active-field snippet)) (yas/field-end (yas/snippet-active-field snippet)))))
    (dolist (field (yas/snippet-fields snippet))
      (princ (format "\tfield %d from %s to %s covering \"%s\" adj-fields %s adj-mirrors %s\n"
		     (yas/field-number field)
		     (marker-position (yas/field-start field))
		     (marker-position (yas/field-end field))
		     (buffer-substring-no-properties (yas/field-start field) (yas/field-end field))
		     (length (yas/field-back-adjacent-fields field))
		     (length (yas/field-back-adjacent-mirrors field))))
      (dolist (mirror (yas/field-mirrors field))
	(princ (format "\t\tmirror from %s to %s covering \"%s\"\n"
		       (marker-position (yas/mirror-start mirror))
		       (marker-position (yas/mirror-end mirror))
		       (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror)))))))

  

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
  (yas/global-mode -1)
  (yas/minor-mode -1)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(defun yas/debug-test (&optional quiet)
  (interactive "P")
  (yas/load-directory (or (and (listp yas/root-directory)
			       (first yas/root-directory))
			  yas/root-directory
			  "~/Source/yasnippet/snippets/"))
  ;;(kill-buffer (get-buffer "*YAS TEST*"))
  (set-buffer (switch-to-buffer "*YAS TEST*"))
  (mapcar #'yas/commit-snippet (yas/snippets-at-point 'all-snippets))
  (erase-buffer)
  (setq buffer-undo-list nil)
  (setq undo-in-progress nil)
  (snippet-mode)
  (yas/minor-mode 1)
  (let ((abbrev))
    ;; (if (require 'ido nil t)
    ;; 	(setq abbrev (ido-completing-read "Snippet abbrev: " '("crazy" "prip" "prop")))
    ;;   (setq abbrev "prop"))
  (setq abbrev "$f")
  (insert abbrev))
  (unless quiet
    (add-hook 'post-command-hook 'yas/debug-some-vars 't 'local)))

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
