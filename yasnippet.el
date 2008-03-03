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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")

(defstruct yas/snippet
  "The snippet structure of yasnippet."
  overlay fields exit-marker)
(defstruct yas/snippet-field
  "The snippet-field structure of yasnippet."
  overlay state)

(defconst yas/escape-backslash
  (concat "YASESCAPE" "BACKSLASH" "PROTECTGUARD"))
(defconst yas/escape-dollar
  (concat "YASESCAPE" "DOLLAR" "PROTECTGUARD"))
(defconst yas/escape-backquote
  (concat "YASESCAPE" "BACKQUOTE" "PROTECTGUARD"))

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

(defun yas/create-snippet (template 
			   indent? column tabify? tab-width)
  "Create a snippet according to TEMPLATE. Each line is indented to
current column if `yas/indent-line' is non-`nil'."
  (with-temp-buffer
    (insert template)
    
    ;; Step 1: do necessary indent
    (when indent?
      (let* ((indent (if tabify?
			 (concat (make-string (/ column tab-width) ?\t)
				 (make-string (% column tab-width) ?\ ))
		       (make-string column ?\ ))))
	(goto-char (point-min))
	(while (zerop (forward-line))
	  (insert indent)
	  (end-of-line))))

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

    ;; Step : restore all escape characters
    (yas/replace-all yas/escape-dollar "$")
    (yas/replace-all yas/escape-backquote "`")
    (yas/replace-all yas/escape-backslash "\\")

    (buffer-string)))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (goto-char start)
  (insert (yas/create-snippet template
			      yas/indent-line   ; indent?
			      (current-column)	; column
			      indent-tabs-mode  ; tabify?
			      tab-width		; tab-width
			      ))
  (delete-char (- end start)))


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

(provide 'yasnippet)
