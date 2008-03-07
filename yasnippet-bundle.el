;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;; 
;; Author: pluskid <pluskid@gmail.com>
;; Version: 0.1
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
;;   3. Place the `snippets' directory somewhere. E.g: ~/.emacs.d/snippets
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
(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/root-directory nil
  "The root directory that stores the snippets for each major modes.")

(defvar yas/indent-line t
  "Each (except the 1st) line of the snippet template is indented to
current column if this variable is non-`nil'.")
(make-variable-buffer-local 'yas/indent-line)

(defvar yas/trigger-keys (list (kbd "TAB"))
  "The keys to bind as a trigger of snippet.")
(defvar yas/trigger-fallback 'indent-according-to-mode
  "The fallback command to call when there's no snippet to expand.")
(make-variable-buffer-local 'yas/trigger-fallback)

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")
(define-key yas/keymap (kbd "TAB") 'yas/next-field-group)
(define-key yas/keymap (kbd "S-TAB") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-iso-lefttab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-tab>") 'yas/prev-field-group)

(defvar yas/use-menu t
  "If this is set to `t', all snippet template of the current
mode will be listed under the menu \"yasnippet\".")
(defvar yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/version "0.1")

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
(define-key yas/menu-keymap [yas/separator]
  '(menu-item "--"))

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
  "Contains the next id for a snippet")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (yas/template (:constructor yas/make-template (content name)))
  "A template for a snippet."
  content
  name)
(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet."
  (groups nil)
  (tabstops nil)    ; tabstops are those groups whose init value is empty
  (exit-marker nil)
  (id (yas/snippet-next-id) :read-only t)
  (overlay nil))
(defstruct (yas/group (:constructor yas/make-group (primary-field snippet)))
  "A group contains a list of field with the same number."
  primary-field
  (fields (list primary-field))
  (next nil)
  (prev nil)
  snippet)
(defstruct (yas/field (:constructor yas/make-field (overlay number value)))
  "A field in a snippet."
  overlay
  number
  value)

(defun yas/snippet-add-field (snippet field)
  "Add FIELD to SNIPPET."
  (let ((group (find field
		     (yas/snippet-groups snippet)
		     :test
		     '(lambda (field group)
			(and (not (null (yas/field-number field)))
			     (= (yas/field-number field)
				(yas/group-number group)))))))
    (if group
	(yas/group-add-field group field)
      (push (yas/make-group field snippet)
	    (yas/snippet-groups snippet)))))

(defun yas/group-value (group)
  "Get the default value of the field group."
  (or (yas/field-value
       (yas/group-primary-field group))
      ""))
(defun yas/group-number (group)
  "Get the number of the field group."
  (yas/field-number
   (yas/group-primary-field group)))
(defun yas/group-add-field (group field)
  "Add a field to the field group. If the value of the primary 
field is nil and that of the field is not nil, the field is set
as the primary field of the group."
  (push field (yas/group-fields group))
  (when (and (null (yas/field-value (yas/group-primary-field group)))
	     (yas/field-value field))
    (setf (yas/group-primary-field group) field)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the start point of the overlay."
  (let ((n1 (yas/field-number field1))
	(n2 (yas/field-number field2)))
    (if n1
	(if n2
	    (< n1 n2)
	  t)
      (if n2
	  nil
	(< (overlay-start (yas/field-overlay field1))
	   (overlay-start (yas/field-overlay field2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/eval-string (string)
  "Evaluate STRING and convert the result to string."
  (condition-case err
      (format "%s" (eval (read string)))
    (error (format "(error in elisp evaluation: %s)" 
		   (error-message-string err)))))
(defsubst yas/replace-all (from to)
  "Replace all occurance from FROM to TO."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun yas/snippet-table (mode)
  "Get the snippet table corresponding to MODE."
  (let ((table (gethash mode yas/snippet-tables)))
    (unless table
      (setq table (make-hash-table :test 'equal))
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
	syntax done)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
	(skip-syntax-backward syntax)
	(when (gethash (buffer-substring-no-properties (point) end)
		       (yas/current-snippet-table))
	  (setq done t)
	  (setq start (point)))))
    (list (buffer-substring-no-properties start end)
	  start
	  end)))

(defun yas/synchronize-fields (field-group)
  "Update all fields' text according to the primary field."
  (save-excursion
    (let* ((inhibit-modification-hooks t)
	   (primary (yas/group-primary-field field-group))
	   (primary-overlay (yas/field-overlay primary))
	   (text (buffer-substring-no-properties (overlay-start primary-overlay)
						 (overlay-end primary-overlay))))
      (dolist (field (yas/group-fields field-group))
	(let* ((field-overlay (yas/field-overlay field))
	       (original-length (- (overlay-end field-overlay)
				   (overlay-start field-overlay))))
	  (unless (eq field-overlay primary-overlay)
	    (goto-char (overlay-start field-overlay))
	    (insert text)
	    (if (= (overlay-start field-overlay)
		   (overlay-end field-overlay))
		(move-overlay field-overlay
			      (overlay-start field-overlay)
			      (point))
	      (delete-char original-length))))))))
  
(defun yas/overlay-modification-hook (overlay after? beg end &optional length)
  "Modification hook for snippet field overlay."
  (when (and after? (not undo-in-progress))
    (yas/synchronize-fields (overlay-get overlay 'yas/group))))
(defun yas/overlay-insert-in-front-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted in front of a snippet field."
  (when after?
    (let ((field-group (overlay-get overlay 'yas/group))
	  (inhibit-modification-hooks t))
      (when (not (overlay-get overlay 'yas/modified?))
	(overlay-put overlay 'yas/modified? t)
	(when (> (overlay-end overlay) end)
	  (save-excursion
	    (goto-char end)
	    (delete-char (- (overlay-end overlay) end)))))
     (yas/synchronize-fields field-group))))
(defun yas/overlay-insert-behind-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted just behind a snippet field."
  (when (and after?
	     (null (yas/current-snippet-overlay beg))) ; not inside another field
    (move-overlay overlay
		  (overlay-start overlay)
		  end)
    (yas/synchronize-fields (overlay-get overlay 'yas/group))))

(defun yas/undo-expand-snippet (start end key snippet)
  "Undo a snippet expansion. Delete the overlays. This undo can't be
redo-ed."
  (let ((undo (car buffer-undo-list)))
    (while (null undo)
      (setq buffer-undo-list (cdr buffer-undo-list))
      (setq undo (car buffer-undo-list)))
    ;; Remove this undo operation record
    (setq buffer-undo-list (cdr buffer-undo-list))
  (let ((inhibit-modification-hooks t)
	(buffer-undo-list t))
    (yas/exit-snippet snippet)
    (goto-char start)
    (delete-char (- end start))
    (insert key))))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (goto-char start)

  (let ((key (buffer-substring-no-properties start end))
	(original-undo-list buffer-undo-list)
	(inhibit-modification-hooks t)
	(length (- end start))
	(column (current-column)))
    (save-restriction
      (narrow-to-region start start)

      (setq buffer-undo-list t)
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
	(replace-match (yas/eval-string (match-string-no-properties 1))
		       t t))

      ;; Step 4: protect all escapes, including backslash and backquot
      ;; which may be produced in Step 3
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)
      (yas/replace-all "\\$" yas/escape-dollar)

      (let ((snippet (yas/make-snippet)))
	;; Step 5: Create fields
	(goto-char (point-min))
	(while (re-search-forward yas/field-regexp nil t)
	  (let ((number (or (match-string-no-properties 1)
			    (match-string-no-properties 2)))
		(value (match-string-no-properties 3)))
	    (if (and number
		     (string= "0" number))
		(progn
		  (replace-match "")
		  (setf (yas/snippet-exit-marker snippet)
			(copy-marker (point) t)))
	      (yas/snippet-add-field
	       snippet
	       (yas/make-field
		(make-overlay (match-beginning 0) (match-end 0))
		(and number (string-to-number number))
		value)))))

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
	  (setf (yas/snippet-overlay snippet) overlay))
	
	;; Step 8: Replace fields with default values
	(dolist (group (yas/snippet-groups snippet))
	  (let ((value (yas/group-value group)))
	    (when (string= "" value)
	      (push group (yas/snippet-tabstops snippet)))
	    (dolist (field (yas/group-fields group))
	      (let* ((overlay (yas/field-overlay field))
		     (start (overlay-start overlay))
		     (end (overlay-end overlay))
		     (length (- end start)))
		(goto-char start)
		(insert value)
		(delete-char length)))))

	;; Step 9: restore all escape characters
	(yas/replace-all yas/escape-dollar "$")
	(yas/replace-all yas/escape-backquote "`")
	(yas/replace-all yas/escape-backslash "\\")

	;; Step 10: Set up properties of overlays
	(dolist (group (yas/snippet-groups snippet))
	  (let ((overlay (yas/field-overlay
			  (yas/group-primary-field group))))
	    (overlay-put overlay 'yas/snippet snippet)
	    (overlay-put overlay 'yas/group group)
	    (overlay-put overlay 'yas/modified? nil)
	    (overlay-put overlay 'modification-hooks yas/overlay-modification-hooks)
	    (overlay-put overlay 'insert-in-front-hooks yas/overlay-insert-in-front-hooks)
	    (overlay-put overlay 'insert-behind-hooks yas/overlay-insert-behind-hooks)
	    (dolist (field (yas/group-fields group))
	      (overlay-put (yas/field-overlay field)
			   'face 
			   'highlight))))

	;; Step 11: move to end and make sure exit-marker exist
	(goto-char (point-max))
	(unless (yas/snippet-exit-marker snippet)
	  (setf (yas/snippet-exit-marker snippet) (copy-marker (point) t)))

	;; Step 12: Construct undo information
	(unless (eq original-undo-list t)
	  (add-to-list 'original-undo-list
		       `(apply yas/undo-expand-snippet
			       ,(point-min)
			       ,(point-max)
			       ,key
			       ,snippet)))

	;; Step 13: remove the trigger key
	(widen)
	(delete-char length)

	(setq buffer-undo-list original-undo-list)

	;; Step 14: place the cursor at a proper place
	(let ((groups (yas/snippet-groups snippet))
	      (exit-marker (yas/snippet-exit-marker snippet)))
	  (if groups
	      (goto-char (overlay-start 
			  (yas/field-overlay
			   (yas/group-primary-field
			    (car groups)))))
	    ;; no need to call exit-snippet, since no overlay created.
	    (yas/exit-snippet snippet)))))))

(defun yas/current-snippet-overlay (&optional point)
  "Get the most proper overlay which is belongs to a snippet."
  (let ((point (or point (point)))
	(snippet-overlay nil))
    (dolist (overlay (overlays-at point))
      (when (overlay-get overlay 'yas/snippet)
	(if (null snippet-overlay)
	    (setq snippet-overlay overlay)
	  (when (> (yas/snippet-id (overlay-get overlay 'yas/snippet))
		   (yas/snippet-id (overlay-get snippet-overlay 'yas/snippet)))
	    (setq snippet-overlay overlay)))))
    snippet-overlay))

(defun yas/snippet-of-current-keymap (&optional point)
  "Get the snippet holding the snippet keymap under POINT."
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

(defun yas/current-overlay-for-navigation ()
  "Get current overlay for navigation. Might be overlay at current or previous point."
  (let ((overlay1 (yas/current-snippet-overlay))
	(overlay2 (if (bobp)
		      nil
		    (yas/current-snippet-overlay (- (point) 1)))))
    (if (null overlay1)
	overlay2
      (if (or (null overlay2)
	      (eq (overlay-get overlay1 'yas/snippet) 
		  (overlay-get overlay2 'yas/snippet)))
	  overlay1
	(if (> (yas/snippet-id (overlay-get overlay2 'yas/snippet))
	       (yas/snippet-id (overlay-get overlay1 'yas/snippet)))
	    overlay2
	  overlay1)))))

(defun yas/navigate-group (group next?)
  "Go to next of previous field group. Exit snippet if none."
  (let ((target (if next?
		    (yas/group-next group)
		  (yas/group-prev group))))
    (if target
	(goto-char (overlay-start
		    (yas/field-overlay
		     (yas/group-primary-field target))))
      (yas/exit-snippet (yas/group-snippet group)))))

(defun yas/parse-template ()
  "Parse the template in the current buffer.
If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Currently only the \"name\" variable is recognized. Here's 
an example:

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let (template name bound)
    (if (re-search-forward "^# --\n" nil t)
	(progn (setq template 
		     (buffer-substring-no-properties (point) 
						     (point-max)))
	       (setq bound (point))
	       (goto-char (point-min))
	       (while (re-search-forward "^#\\([^ ]+\\) *: *\\(.*\\)$" bound t)
		 (when (string= "name" (match-string-no-properties 1))
		   (setq name (match-string-no-properties 2)))))
      (setq template
	    (buffer-substring-no-properties (point-min) (point-max))))
    (list template name)))

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
  (if (boundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (or point (point))))))
	(list (list (+ (car x-y) 10)
		    (+ (cdr x-y) 20))
	      (selected-window)))
    t))
 
(defun yas/popup-for-template (templates)
  "Show a popup menu listing templates to let the user select one."
  (if window-system
      (car (x-popup-menu (yas/point-to-coord) (yas/fake-keymap-for-popup templates)))
    ;; no window system, simply select the first one
    (cdar templates)))

(defun yas/load-directory-1 (directory)
  "Really do the job of loading snippets from a directory 
hierarchy."
  (with-temp-buffer
    (dolist (mode (yas/directory-files directory nil))
      (let ((mode-sym (intern (file-name-nondirectory mode))))
	(dolist (file (yas/directory-files mode t))
	  (when (file-readable-p file)
	    (insert-file-contents file nil nil nil t)
	    (multiple-value-bind 
		(key template name)
		(cons (file-name-nondirectory file)
		      (yas/parse-template))
	      (yas/define mode-sym key template name))))))))

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
      (yas/load-directory-1 yas/root-directory)
    (call-interactively 'yas/load-directory))
  (message "done."))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.
Below the top-level directory, each directory is a mode
name. And under each subdirectory, each file is a definition
of a snippet. The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless yas/root-directory
    (setq yas/root-directory directory))
  (yas/load-directory-1 directory)
  (when (interactive-p)
    (message "done.")))

(defun yas/initialize ()
  "Do necessary initialization."
  (dolist (key yas/trigger-keys)
    (global-set-key key 'yas/expand))
  (when yas/use-menu
    (define-key-after 
      (lookup-key global-map [menu-bar])
      [yasnippet]
      (cons "YASnippet" yas/menu-keymap)
      'buffer)))

(defun yas/define (mode key template &optional name)
  "Define a snippet. Expanding KEY into TEMPLATE.
NAME is a description to this template. Also update
the menu if `yas/use-menu' is `t'."
  (let* ((full-key key)
	 (key (file-name-sans-extension full-key))
	 (template (yas/make-template template (or name key)))
	 (snippet-table (yas/snippet-table mode)))
    (puthash key
	     (yas/modify-alist (gethash key snippet-table)
			       full-key
			       template)
	     snippet-table)
    (when yas/use-menu
      (let ((keymap (yas/menu-keymap-for-mode mode)))
	(define-key yas/menu-keymap (vector mode) 
	  `(menu-item ,(symbol-name mode) ,keymap))
	(define-key keymap (vector (make-symbol full-key))
	  `(menu-item ,(yas/template-name template)
		      ,(yas/make-menu-binding (yas/template-content template))
		      :keys ,(concat key yas/trigger-symbol)))))))

(defun yas/expand ()
  "Expand a snippet."
  (interactive)
  (multiple-value-bind (key start end) (yas/current-key)
    (let ((templates (gethash key (yas/current-snippet-table))))
      (if templates
	  (let ((template (if (null (cdr templates)) ; only 1 template
			      (yas/template-content (cdar templates))
			    (yas/popup-for-template templates))))
	    (when template
	      (yas/expand-snippet start end template)))
	(when yas/trigger-fallback
	  (call-interactively yas/trigger-fallback))))))

(defun yas/next-field-group ()
  "Navigate to next field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
	(yas/navigate-group (overlay-get overlay 'yas/group) t)
      (let ((snippet (yas/snippet-of-current-keymap))
	    (done nil))
	(if snippet
	  (do* ((tabstops (yas/snippet-tabstops snippet) (cdr tabstops))
		(tabstop (car tabstops) (car tabstops)))
	      ((or (null tabstops)
		   done)
	       (unless done (message "Not in a snippet field.")))
	    (when (= (point)
		     (overlay-start
		      (yas/field-overlay
		       (yas/group-primary-field tabstop))))
	      (setq done t)
	      (yas/navigate-group tabstop t)))
	  (message "Not in a snippet field."))))))

(defun yas/prev-field-group ()
  "Navigate to prev field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
	(yas/navigate-group (overlay-get overlay 'yas/group) nil)
      (let ((snippet (yas/snippet-of-current-keymap))
	    (done nil))
	(if snippet
	  (do* ((tabstops (yas/snippet-tabstops snippet) (cdr tabstops))
		(tabstop (car tabstops) (car tabstops)))
	      ((or (null tabstops)
		   done)
	       (unless done (message "Not in a snippet field.")))
	    (when (= (point)
		     (overlay-start
		      (yas/field-overlay
		       (yas/group-primary-field tabstop))))
	      (setq done t)
	      (yas/navigate-group tabstop nil)))
	  (message "Not in a snippet field."))))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and delete the snippet."
  (interactive)
  (goto-char (yas/snippet-exit-marker snippet))
  (delete-overlay (yas/snippet-overlay snippet))
  (dolist (group (yas/snippet-groups snippet))
    (dolist (field (yas/group-fields group))
      (delete-overlay (yas/field-overlay field)))))


(provide 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)

;;; snippets from ../snippets

;; snippets for c++-mode
(yas/define 'c++-mode "beginend"
  "${1:v}.begin(), $1.end" "v.begin(), v.end()")
(yas/define 'c++-mode "class"
  "class ${1:Name}
{
public:
    $1($2);
    virtual ~$1();
};" "class ... { ... }")
(yas/define 'c++-mode "do"
  "do
{
    $0
} while (${1:condition});" "do { ... } while (...)")
(yas/define 'c++-mode "for"
  "for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}" "for (...; ...; ...) { ... }")
(yas/define 'c++-mode "if"
  "if (${1:condition})
{
    $0
}" "if (...) { ... }")
(yas/define 'c++-mode "inc"
  "#include \"$1\"" "#include \"...\"")
(yas/define 'c++-mode "inc.1"
  "#include <$1>" "#include <...>")
(yas/define 'c++-mode "main"
  "int main(int argc, char const *argv)
{
    $0
    return 0;
}" "int main(argc, argv) { ... }")
(yas/define 'c++-mode "once"
  "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */" "#ifndef XXX; #define XXX; #endif")
(yas/define 'c++-mode "struct"
  "struct ${1:name}
{
    $0
};" "struct ... { ... }")

;; snippets for c-mode
(yas/define 'c-mode "do"
  "do
{
    $0
} while (${1:condition});" "do { ... } while (...)")
(yas/define 'c-mode "for"
  "for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}" "for (...; ...; ...) { ... }")
(yas/define 'c-mode "if"
  "if (${1:condition})
{
    $0
}" "if (...) { ... }")
(yas/define 'c-mode "inc"
  "#include \"$1\"" "#include \"...\"")
(yas/define 'c-mode "inc.1"
  "#include <$1>" "#include <...>")
(yas/define 'c-mode "main"
  "int main(int argc, char *argv)
{
    $0
    return 0;
}" "int main(argc, argv) { ... }")
(yas/define 'c-mode "once"
  "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */" "#ifndef XXX; #define XXX; #endif")
(yas/define 'c-mode "struct"
  "struct ${1:name}
{
    $0
};" "struct ... { ... }")

;; snippets for cperl-mode
(yas/define 'cperl-mode "eval"
  "eval {
    ${1:# do something risky...}
};
if (\\$@) {
    ${2:# handle failure...}
}" "eval { ... } if ($@) { ... }")
(yas/define 'cperl-mode "for"
  "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {
    ${3:# body...}
}" "for (...) { ... }")
(yas/define 'cperl-mode "fore"
  "foreach my \\$${1:x} (@${2:array}) {
    ${3:# body...}
}" "foreach ... { ... }")
(yas/define 'cperl-mode "if"
  "if ($1) {
    $0
}" "if (...) { ... }")
(yas/define 'cperl-mode "ife"
  "if ($1) {
    $2
} else {
    $3
}" "if (...) { ... } else { ... }")
(yas/define 'cperl-mode "ifee"
  "if ($1) {
	${2:# body...}
} elsif ($3) {
	${4:# elsif...}
} else {
	${5:# else...}
}" "if, elsif, else ...")
(yas/define 'cperl-mode "sub"
  "sub ${1:function_name} {
    $0
}" "sub ... { ... }")
(yas/define 'cperl-mode "unless"
  "unless ($1) {
    $0
}" "unless (...) { ... }")
(yas/define 'cperl-mode "while"
  "while ($1) {
    $0
}" "while (...) { ... }")
(yas/define 'cperl-mode "xfore"
  "${1:expression} foreach @${2:array};" "... foreach ...")
(yas/define 'cperl-mode "xif"
  "${1:expression} if ${2:condition}" "... if ...")
(yas/define 'cperl-mode "xunless"
  "${1:expression} unless ${2:condition}" "... unless ...")
(yas/define 'cperl-mode "xwhile"
  "${1:expression} while ${2:condition};" "... while ...")

;; snippets for css-mode
(yas/define 'css-mode "background"
  "background-color: #${1:DDD};" "background-color: ...")
(yas/define 'css-mode "background.1"
  "background-image: url($1);" "background-image: ...")
(yas/define 'css-mode "border"
  "border: ${1:1px} ${2:solid} #${3:999};" "border size style color")

;; snippets for html-mode
(yas/define 'html-mode "div"
  "<div$1>$0</div>" "<div...>...</div>")
(yas/define 'html-mode "doctype"
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict")
(yas/define 'html-mode "doctype.xhml1"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset")
(yas/define 'html-mode "doctype.xhtml1_1"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1")
(yas/define 'html-mode "doctype.xhtml1_strict"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict")
(yas/define 'html-mode "doctype.xhtml1_transitional"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional")

;; snippets for python-mode
(yas/define 'python-mode "for"
  "for ${var} in ${collection}:
    $0" "for ... in ... : ...")
(yas/define 'python-mode "ifmain"
  "if __name__ == '__main__':
    $0" "if __name__ == '__main__': ...")
(yas/define 'python-mode "while"
  "while ${condition}:
    $0" "while ... : ...")
(yas/define 'python-mode "__"
  "__${init}__" "__...__")

;; snippets for ruby-mode
(yas/define 'ruby-mode "#"
  "# => " "# =>")
(yas/define 'ruby-mode "=b"
  "=begin rdoc
  $0
=end" "=b")
(yas/define 'ruby-mode "all"
  "all? { |${e}| $0 }" "all? { |...| ... }")
(yas/define 'ruby-mode "am"
  "alias_method :${new_name}, :${old_name}" "alias_method new, old")
(yas/define 'ruby-mode "any"
  "any? { |${e}| $0 }" "any? { |...| ... }")
(yas/define 'ruby-mode "app"
  "if __FILE__ == $PROGRAM_NAME
  $0
end" "if __FILE__ == $PROGRAM_NAME ... end")
(yas/define 'ruby-mode "bm"
  "Benchmark.bmbm(${1:10}) do |x|
  $0
end" "Benchmark.bmbm(...) do ... end")
(yas/define 'ruby-mode "case"
  "case ${1:object}
when ${2:condition}
  $0
end" "case ... end")
(yas/define 'ruby-mode "cla"
  "class << ${self}
  $0
end" "class << self ... end")
(yas/define 'ruby-mode "classify"
  "classify { |${e}| $0 }" "classify { |...| ... }")
(yas/define 'ruby-mode "cls"
  "class ${Name}
  $0
end" "class ... end")
(yas/define 'ruby-mode "collect"
  "collect { |${e}| $0 }" "collect { |...| ... }")
(yas/define 'ruby-mode "Comp"
  "include Comparable

def <=> other
  $0
end" "include Comparable; def <=> ... end")
(yas/define 'ruby-mode "dee"
  "Marshal.load(Marshal.dump($0))" "deep_copy(...)")
(yas/define 'ruby-mode "deli"
  "delete_if { |${e} $0 }" "delete_if { |...| ... }")
(yas/define 'ruby-mode "det"
  "detect { |${e}| $0 }" "detect { |...| ... }")
(yas/define 'ruby-mode "ea"
  "each { |${e}| $0 }" "each { |...| ... }")
(yas/define 'ruby-mode "eac"
  "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }")
(yas/define 'ruby-mode "eai"
  "each_index { |${i}| $0 }" "each_index { |i| ... }")
(yas/define 'ruby-mode "eav"
  "each_value { |${val}| $0 }" "each_value { |val| ... }")
(yas/define 'ruby-mode "eawi"
  "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }")
(yas/define 'ruby-mode "forin"
  "for ${1:element} in ${2:collection}
  $0
end" "for ... in ...; ... end")
(yas/define 'ruby-mode "if"
  "if ${1:condition}
  $0
end" "if ... end")
(yas/define 'ruby-mode "ife"
  "if ${1:condition}
  $2
else
  $3
end" "if ... else ... end")
(yas/define 'ruby-mode "inject"
  "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }")
(yas/define 'ruby-mode "mm"
  "def method_missing(method, *args)
  $0
end" "def method_missing ... end")
(yas/define 'ruby-mode "r"
  "attr_reader :${attr_names}" "attr_reader ...")
(yas/define 'ruby-mode "rb"
  "#!/usr/bin/ruby -wKU
" "/usr/bin/ruby -wKU")
(yas/define 'ruby-mode "reject"
  "reject { |${1:element}| $0 }" "reject { |...| ... }")
(yas/define 'ruby-mode "req"
  "require \"$0\"" "require \"...\"")
(yas/define 'ruby-mode "rreq"
  "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)")
(yas/define 'ruby-mode "rw"
  "attr_accessor :{attr_names}" "attr_accessor ...")
(yas/define 'ruby-mode "select"
  "select { |${1:element}| $0 }" "select { |...| ... }")
(yas/define 'ruby-mode "w"
  "attr_writer :${attr_names}" "attr_writer ...")
(yas/define 'ruby-mode "y"
  ":yields: $0" ":yields: arguments (rdoc)")
(yas/define 'ruby-mode "zip"
  "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }")

;; snippets for text-mode
(yas/define 'text-mode "email"
  "`user-mail-address`" "(user's email)")
(yas/define 'text-mode "time"
  "`(current-time-string)`" "(current time)")
(provide 'yasnippet-bundle)
