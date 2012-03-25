;;; yasnippet-tests.el --- some yasnippet tests

;; Copyright (C) 2012  João Távora

;; Author: João Távora <joaot@siscog.pt>
;; Keywords: emulations, convenience

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

;; Attempt to test basic snippet mechanics and the loading system 

;;; Code:

(require 'yasnippet)
(require 'ert)
(require 'ert-x)


;;; Snippet mechanics

(ert-deftest field-navigation ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${1:brother} from another ${2:mother}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother"))
    
    (should (looking-at "brother"))
    (ert-simulate-command '(yas/next-field-or-maybe-expand))
    (should (looking-at "mother"))
    (ert-simulate-command '(yas/prev-field))
    (should (looking-at "brother"))))

(ert-deftest simple-mirror ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${1:brother} from another $1")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another brother"))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "bla from another bla"))))

(ert-deftest mirror-with-transformation ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${1:brother} from another ${1:$(upcase yas/text)}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another BROTHER"))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "bla from another BLA"))))

(ert-deftest nested-placeholders-kill-superfield ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "brother from ${2:another ${3:mother}}!")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother!"))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from bla!"))))

(ert-deftest nested-placeholders-use-subfield ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "brother from ${2:another ${3:mother}}!")
    (ert-simulate-command '(yas/next-field-or-maybe-expand))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another bla!"))))

;; (ert-deftest in-snippet-undo ()
;;   (with-temp-buffer
;;     (yas/minor-mode 1)
;;     (yas/expand-snippet "brother from ${2:another ${3:mother}}!")
;;     (ert-simulate-command '(yas/next-field-or-maybe-expand))
;;     (ert-simulate-command `(yas/mock-insert "bla"))
;;     (ert-simulate-command '(undo))
;;     (should (string= (buffer-substring-no-properties (point-min) (point-max))
;;                      "brother from another mother!"))))


;;; Misc tests
;;; 

(ert-deftest protection-overlay-no-cheating ()
  "Protection overlays at the very end of the buffer, are dealt by cheatingly inserting a newline!

TODO: correct this bug!"
  :expected-result :failed
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${2:brother} from another ${1:mother}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother") ;; no newline should be here!
            )))

;;; Loading
;;;
(ert-deftest basic-loading ()
  "Test basic loading and expansion of snippets"
  (yas/saving-variables
   (with-snippet-dirs
    '((".emacs.d/snippets"
       ("c-mode"
        (".yas-parents" . "cc-mode")
        ("printf" . "printf($1);"))
       ("emacs-lisp-mode" ("ert-deftest" . "(ert-deftest ${1:name} () $0)"))
       ("lisp-interaction-mode" (".yas-parents" . "emacs-lisp-mode")))
      ("library/snippets"
       ("c-mode" (".yas-parents" . "c++-mode"))
       ("cc-mode" ("def" . "# define"))
       ("emacs-lisp-mode" ("dolist" . "(dolist)"))
       ("lisp-interaction-mode" ("sc" . "brother from another mother"))))
    (yas/reload-all)
    (with-temp-buffer
      (lisp-interaction-mode)
      (yas/minor-mode 1)
      (insert "sc")
      (ert-simulate-command '(yas/expand))
      (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       "brother from another mother"))))))



;;; Helpers
;;; 

(defun yas/mock-insert (string)
  (interactive)
  (do ((i 0 (1+ i)))
      ((= i (length string)))
    (insert (aref string i))))

(defun yas/make-file-or-dirs (ass)
  (let ((file-or-dir-name (car ass))
        (content (cdr ass)))
    (cond ((listp content)
           (make-directory file-or-dir-name 'parents)
           (let ((default-directory (concat default-directory "/" file-or-dir-name)))
             (mapc #'yas/make-file-or-dirs content)))
          ((stringp content)
           (with-current-buffer (find-file file-or-dir-name)
             (insert content)
             (save-buffer)
             (kill-buffer)))
          (t
           (message "[yas] oops don't know this content")))))


(defun yas/variables ()
  (let ((syms))
    (mapatoms #'(lambda (sym)
                  (if (and (string-match "^yas/[^/]" (symbol-name sym))
                           (boundp sym))
                      (push sym syms))))
    syms))


(defmacro yas/saving-variables (&rest body)
  `(let ,(mapcar #'(lambda (sym)
                     `(,sym ,sym))
                 (yas/variables))
     ,@body))

(defmacro with-snippet-dirs (dirs &rest body)
  `(let ((default-directory (make-temp-file "yasnippet-fixture" t)))
     (setq yas/snippet-dirs ',(mapcar #'car (cadr dirs)))
     (mapc #'yas/make-file-or-dirs ,dirs)
     ,@body))


(provide 'yasnippet-tests)
;;; yasnippet-tests.el ends here
