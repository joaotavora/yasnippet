;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Author: pluskid <pluskid@gmail.com>
;; Version: 0.1

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

;; Nothing.

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/key-syntax "w"
  "Syntax of a key. This is used to determine the current key being 
expanded.")

(defvar yas/indent-line t
  "Each (except the 1st) line of the snippet template is indented to
current column if this variable is non-`nil'.")
(make-variable-buffer-local 'yas/indent-line)

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")
(define-key yas/keymap (kbd "TAB") 'yas/next-field-group)
(define-key yas/keymap (kbd "S-TAB") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-iso-lefttab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-tab>") 'yas/prev-field-group)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")

(defconst yas/escape-backslash
  (concat "YASESCAPE" "BACKSLASH" "PROTECTGUARD"))
(defconst yas/escape-dollar
  (concat "YASESCAPE" "DOLLAR" "PROTECTGUARD"))
(defconst yas/escape-backquote
  (concat "YASESCAPE" "BACKQUOTE" "PROTECTGUARD"))

(defconst yas/field-regexp
  (concat "$\\(?1:[0-9]+\\)" "\\|"
	  "${\\(?:\\(?1:[0-9]+\\):\\)?\\(?2:[^}]*\\)}"))

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet")
(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

(defun yas/snippet-new ()
  "Create a new snippet."
  (cons nil (cons nil (yas/snippet-next-id))))
(defun yas/snippet-field-groups (snippet)
  "Get field groups of SNIPPET."
  (car snippet))
(defun yas/snippet-field-groups-set (snippet groups)
  "Set field groups of SNIPPET."
  (setf (car snippet) groups))
(defun yas/snippet-exit-marker-set (snippet marker)
  "Set exit marker of SNIPPET."
  (setf (cadr snippet) marker))
(defun yas/snippet-exit-marker (snippet)
  "Get exit marker of SNIPPET."
  (cadr snippet))
(defun yas/snippet-id (snippet)
  "Get id of the snippet."
  (cddr snippet))
(defun yas/snippet-add-field (snippet field)
  "Add FIELD to SNIPPET."
  (let ((group (find field
		     (yas/snippet-field-groups snippet)
		     :test
		     '(lambda (field group)
			(= (yas/snippet-field-number field)
			   (yas/snippet-field-group-number group))))))
    (if group
	(yas/snippet-field-group-add group field)
      (push (yas/snippet-field-group-new field)
	    (car snippet)))))

(defun yas/snippet-field-group-new (field)
  "Create a new field group."
  (list field             ; primary field
	(list field)      ; fields
	nil		  ; next field group
	nil))		  ; prev field group
(defun yas/snippet-field-group-primary (group)
  "Get the primary field of this group."
  (car group))
(defun yas/snippet-field-group-fields (group)
  "Get all fields belonging to this group."
  (cadr group))
(defun yas/snippet-field-group-set-next (group next)
  "Set next field group of GROUP."
  (setf (nth 2 group) next))
(defun yas/snippet-field-group-next (group)
  "Get next field group."
  (nth 2 group))
(defun yas/snippet-field-group-set-prev (group prev)
  "Set previous field group of GROUP."
  (setf (nth 3 group) prev))
(defun yas/snippet-field-group-prev (group)
  "Get previous field group."
  (nth 3 group))
(defun yas/snippet-field-group-value (group)
  "Get the default value of the field group."
  (or (yas/snippet-field-value
       (yas/snippet-field-group-primary group))
      ""))
(defun yas/snippet-field-group-number (group)
  "Get the number of the field group."
  (yas/snippet-field-number
   (yas/snippet-field-group-primary group)))
(defun yas/snippet-field-group-add (group field)
  "Add a field to the field group. If the value of the primary 
field is nil and that of the field is not nil, the field is set
as the primary field of the group."
  (push field (nth 1 group))
  (when (and (null (yas/snippet-field-value (car group)))
	     (yas/snippet-field-value field))
    (setf (car group) field)))

(defun yas/snippet-field-new (overlay number value)
  "Create a new snippet-field."
  (cons overlay (cons number value)))
(defun yas/snippet-field-overlay (field)
  "Get the overlay of the field."
  (car field))
(defun yas/snippet-field-number (field)
  "Get the number of the field."
  (cadr field))
(defun yas/snippet-field-value (field)
  "Get the value of the field."
  (cddr field))
(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the start point of the overlay."
  (let ((n1 (yas/snippet-field-number field1))
	(n2 (yas/snippet-field-number field2)))
    (if n1
	(if n2
	    (< n1 n2)
	  t)
      (if n2
	  nil
	(< (overlay-start (yas/snippet-field-overlay field1))
	   (overlay-start (yas/snippet-field-overlay field2)))))))

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

(defsubst yas/template (key snippet-table)
  "Get template for KEY in SNIPPET-TABLE."
  (gethash key snippet-table))

(defun yas/current-key ()
  "Get the key under current position."
  (let ((start (point))
	(end (point)))
    (save-excursion
      (skip-syntax-backward yas/key-syntax)
      (setq start (point))
      (list (buffer-substring-no-properties start end)
	    start
	    end))))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (goto-char start)

  (let ((length (- end start))
	(column (current-column)))
  (save-restriction
    (narrow-to-region start start)

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

    (let ((snippet (yas/snippet-new)))
      ;; Step 5: Create fields
      (goto-char (point-min))
      (while (re-search-forward yas/field-regexp nil t)
	(let ((number (match-string-no-properties 1)))
	  (if (and number
		   (string= "0" number))
	      (progn
		(replace-match "")
		(yas/snippet-exit-marker-set
		 snippet
		 (copy-marker (point) t)))
	    (yas/snippet-add-field
	     snippet
	     (yas/snippet-field-new
	      (make-overlay (match-beginning 0) (match-end 0))
	      (and number (string-to-number number))
	      (match-string-no-properties 2))))))

      ;; Step 6: Sort and link each field group
      (yas/snippet-field-groups-set
       snippet
       (sort (yas/snippet-field-groups snippet)
	     '(lambda (group1 group2)
		(yas/snippet-field-compare
		 (yas/snippet-field-group-primary group1)
		 (yas/snippet-field-group-primary group2)))))
      (let ((prev nil))
	(dolist (group (yas/snippet-field-groups snippet))
	  (yas/snippet-field-group-set-prev group prev)
	  (when prev
	    (yas/snippet-field-group-set-next prev group))
	  (setq prev group)))

      ;; Step 7: Set up properties of overlays, including keymaps
      (dolist (group (yas/snippet-field-groups snippet))
	(let ((overlay (yas/snippet-field-overlay
			(yas/snippet-field-group-primary group))))
	  (overlay-put overlay 'keymap yas/keymap)
	  (overlay-put overlay 'yas/snippet snippet)
	  (overlay-put overlay 'yas/snippet-field-group group)
	  (dolist (field (yas/snippet-field-group-fields group))
	    (overlay-put (yas/snippet-field-overlay field)
			 'face 
			 'highlight))))

      ;; Step 8: Replace fields with default values
      (dolist (group (yas/snippet-field-groups snippet))
	(let ((value (yas/snippet-field-group-value group)))
	  (dolist (field (yas/snippet-field-group-fields group))
	    (let* ((overlay (yas/snippet-field-overlay field))
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

      ;; Step 10: move to end and make sure exit-marker exist
      (goto-char (point-max))
      (unless (yas/snippet-exit-marker snippet)
	(yas/snippet-exit-marker-set snippet (copy-marker (point) t)))

      ;; Step 11: remove the trigger key
      (widen)
      (delete-char length)

      ;; Step 12: place the cursor at a proper place
      (let ((groups (yas/snippet-field-groups snippet))
	    (exit-marker (yas/snippet-exit-marker snippet)))
	(if groups
	    (goto-char (overlay-start 
			(yas/snippet-field-overlay
			 (yas/snippet-field-group-primary
			  (car groups)))))
	  ;; no need to call exit-snippet, since no overlay created.
	  (goto-char exit-marker)))))))

(defun yas/current-snippet-overlay ()
  "Get the most proper overlay which is belongs to a snippet."
  (let ((snippet-overlay nil))
    (dolist (overlay (overlays-at (point)))
      (when (overlay-get overlay 'yas/snippet)
	(if (null snippet-overlay)
	    (setq snippet-overlay overlay)
	  (when (> (yas/snippet-id (overlay-get overlay 'yas/snippet))
		   (yas/snippet-id (overlay-get snippet-overlay 'yas/snippet)))
	    (setq snippet-overlay overlay)))))
    snippet-overlay))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/define (mode key template)
  "Define a snippet. Expanding KEY into TEMPLATE."
  (puthash key template (yas/snippet-table mode)))

(defun yas/expand ()
  "Expand a snippet. When a snippet is expanded, t is returned,
otherwise, nil returned."
  (interactive)
  (multiple-value-bind (key start end) (yas/current-key)
    (let ((template (yas/template key (yas/current-snippet-table))))
      (if template
	  (progn
	    (yas/expand-snippet start end template)
	    t)
	nil))))

(defun yas/next-field-group ()
  "Navigate to next field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-snippet-overlay)))
    (if overlay
	(let ((next (yas/snippet-field-group-next 
		     (overlay-get overlay 'yas/snippet-field-group))))
	  (if next
	      (goto-char (overlay-start
			  (yas/snippet-field-overlay
			   (yas/snippet-field-group-primary next))))
	    (yas/exit-snippet (overlay-get overlay 'yas/snippet))))
      (message "Not in a snippet field."))))

(defun yas/prev-field-group ()
  "Navigate to prev field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-snippet-overlay)))
    (if overlay
	(let ((prev (yas/snippet-field-group-prev
		     (overlay-get overlay 'yas/snippet-field-group))))
	  (if prev
	      (goto-char (overlay-start
			  (yas/snippet-field-overlay
			   (yas/snippet-field-group-primary prev))))
	    (yas/exit-snippet (overlay-get overlay 'yas/snippet))))
      (message "Not in a snippet field."))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and delete the snippet."
  (interactive)
  (goto-char (yas/snippet-exit-marker snippet))
  (dolist (group (yas/snippet-field-groups snippet))
    (dolist (field (yas/snippet-field-group-fields group))
      (delete-overlay (yas/snippet-field-overlay field)))))

(provide 'yasnippet)
