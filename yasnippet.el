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

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")

(defvar yas/key-syntax "w"
  "Syntax of a key. This is used to determine the current key being 
expanded.")


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
  (delete-region start end)
  (insert template))

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
