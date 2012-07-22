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

;; Test basic snippet mechanics and the loading system

;;; Code:

(require 'yasnippet)
(require 'ert)
(require 'ert-x)


;;; Snippet mechanics

(ert-deftest field-navigation ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} from another ${2:mother}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother"))

    (should (looking-at "brother"))
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (should (looking-at "mother"))
    (ert-simulate-command '(yas-prev-field))
    (should (looking-at "brother"))))

(ert-deftest simple-mirror ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} from another $1")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another brother"))
    (ert-simulate-command `(yas-mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "bla from another bla"))))

(ert-deftest mirror-with-transformation ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} from another ${1:$(upcase yas-text)}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another BROTHER"))
    (ert-simulate-command `(yas-mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "bla from another BLA"))))

(ert-deftest nested-placeholders-kill-superfield ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "brother from ${2:another ${3:mother}}!")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother!"))
    (ert-simulate-command `(yas-mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from bla!"))))

(ert-deftest nested-placeholders-use-subfield ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "brother from ${2:another ${3:mother}}!")
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (ert-simulate-command `(yas-mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another bla!"))))

;; (ert-deftest in-snippet-undo ()
;;   (with-temp-buffer
;;     (yas-minor-mode 1)
;;     (yas-expand-snippet "brother from ${2:another ${3:mother}}!")
;;     (ert-simulate-command '(yas-next-field-or-maybe-expand))
;;     (ert-simulate-command `(yas-mock-insert "bla"))
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
    (yas-minor-mode 1)
    (yas-expand-snippet "${2:brother} from another ${1:mother}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother") ;; no newline should be here!
            )))

;;; Loading
;;;
(defmacro yas-with-some-interesting-snippet-dirs (&rest body)
  `(yas-saving-variables
    (yas-with-snippet-dirs
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
     ,@body)))

(ert-deftest basic-jit-loading ()
  "Test basic loading and expansion of snippets"
  (yas-with-some-interesting-snippet-dirs
   (yas-reload-all)
   (yas--basic-jit-loading-1)))

(ert-deftest basic-jit-loading-with-compiled-snippets ()
  "Test basic loading and expansion of snippets"
  (yas-with-some-interesting-snippet-dirs
   (yas-reload-all)
   (yas-recompile-all)
   (flet ((yas--load-directory-2
           (&rest dummies)
           (ert-fail "yas--load-directory-2 shouldn't be called when snippets have been compiled")))
     (yas-reload-all)
     (yas--basic-jit-loading-1))))

(defun yas--basic-jit-loading-1 (&optional compile)
  (with-temp-buffer
    (should (= 4 (hash-table-count yas--scheduled-jit-loads)))
    (should (= 0 (hash-table-count yas--tables)))
    (lisp-interaction-mode)
    (yas-minor-mode 1)
    (should (= 2 (hash-table-count yas--scheduled-jit-loads)))
    (should (= 2 (hash-table-count yas--tables)))
    (should (= 1 (hash-table-count (yas--table-uuidhash (gethash 'lisp-interaction-mode yas--tables)))))
    (should (= 2 (hash-table-count (yas--table-uuidhash (gethash 'emacs-lisp-mode yas--tables)))))
    (yas-should-expand '(("sc" . "brother from another mother")
                         ("dolist" . "(dolist)")
                         ("ert-deftest" . "(ert-deftest name () )")))
    (c-mode)
    (yas-minor-mode 1)
    (should (= 0 (hash-table-count yas--scheduled-jit-loads)))
    (should (= 4 (hash-table-count yas--tables)))
    (should (= 1 (hash-table-count (yas--table-uuidhash (gethash 'c-mode yas--tables)))))
    (should (= 1 (hash-table-count (yas--table-uuidhash (gethash 'cc-mode yas--tables)))))
    (yas-should-expand '(("printf" . "printf();")
                         ("def" . "# define")))
    (yas-should-not-expand '("sc" "dolist" "ert-deftest"))))


;;; Menu
;;;
(defmacro yas-with-even-more-interesting-snippet-dirs (&rest body)
  `(yas-saving-variables
    (yas-with-snippet-dirs
     `((".emacs.d/snippets"
        ("c-mode"
         (".yas-make-groups" . "")
         ("printf" . "printf($1);")
         ("foo-group-a"
          ("fnprintf" . "fprintf($1);")
          ("snprintf" . "snprintf($1);"))
         ("foo-group-b"
          ("strcmp" . "strecmp($1);")
          ("strcasecmp" . "strcasecmp($1);")))
        ("lisp-interaction-mode"
         ("ert-deftest" . "# group: barbar\n# --\n(ert-deftest ${1:name} () $0)"))
        ("fancy-mode"
         ("a-guy" . "# uuid: 999\n# --\nyo!")
         ("a-sir" . "# uuid: 12345\n# --\nindeed!")
         ("a-lady" . "# uuid: 54321\n# --\noh-la-la!")
         ("a-beggar" . "# uuid: 0101\n# --\narrrgh!")
         ("an-outcast" . "# uuid: 666\n# --\narrrgh!")
         (".yas-setup.el" . , (pp-to-string
                               '(yas-define-menu 'fancy-mode
                                                 '((yas-ignore-item "0101")
                                                   (yas-item "999")
                                                   (yas-submenu "sirs"
                                                                ((yas-item "12345")))
                                                   (yas-submenu "ladies"
                                                                ((yas-item "54321"))))
                                                 '("666")))))))
     ,@body)))

(ert-deftest test-yas-define-menu ()
  (let ((yas-use-menu t))
    (yas-with-even-more-interesting-snippet-dirs
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'fancy-mode yas--menu-table))))
       (should (eql 4 (length menu)))
       (dolist (item '("a-guy" "a-beggar"))
         (should (find item menu :key #'third :test #'string=)))
       (should-not (find "an-outcast" menu :key #'third :test #'string=))
       (dolist (submenu '("sirs" "ladies"))
         (should (keymapp
                  (fourth
                   (find submenu menu :key #'third :test #'string=)))))
       ))))

(ert-deftest test-group-menus ()
  "Test group-based menus using .yas-make-groups and the group directive"
  (let ((yas-use-menu t))
    (yas-with-even-more-interesting-snippet-dirs
     (yas-reload-all 'no-jit)
     ;; first the subdir-based groups
     ;;
     (let ((menu (cdr (gethash 'c-mode yas--menu-table))))
       (should (eql 3 (length menu)))
       (dolist (item '("printf" "foo-group-a" "foo-group-b"))
         (should (find item menu :key #'third :test #'string=)))
       (dolist (submenu '("foo-group-a" "foo-group-b"))
         (should (keymapp
                  (fourth
                   (find submenu menu :key #'third :test #'string=))))))
     ;; now group directives
     ;;
     (let ((menu (cdr (gethash 'lisp-interaction-mode yas--menu-table))))
       (should (eql 1 (length menu)))
       (should (find "barbar" menu :key #'third :test #'string=))
       (should (keymapp
                (fourth
                 (find "barbar" menu :key #'third :test #'string=))))))))

(ert-deftest test-group-menus-twisted ()
  "Same as similarly named test, but be mean.

TODO: be meaner"
  (let ((yas-use-menu t))
    (yas-with-even-more-interesting-snippet-dirs
     ;; add a group directive conflicting with the subdir and watch
     ;; behaviour
     (with-temp-buffer
       (insert "# group: foo-group-c\n# --\nstrecmp($1)")
       (write-region nil nil (concat (first (yas-snippet-dirs))
                                     "/c-mode/foo-group-b/strcmp")))
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'c-mode yas--menu-table))))
       (should (eql 4 (length menu)))
       (dolist (item '("printf" "foo-group-a" "foo-group-b" "foo-group-c"))
         (should (find item menu :key #'third :test #'string=)))
       (dolist (submenu '("foo-group-a" "foo-group-b" "foo-group-c"))
         (should (keymapp
                  (fourth
                   (find submenu menu :key #'third :test #'string=))))))
     ;; delete the .yas-make-groups file and watch behaviour
     ;;
     (delete-file (concat (first (yas-snippet-dirs))
                          "/c-mode/.yas-make-groups"))
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'c-mode yas--menu-table))))
       (should (eql 5 (length menu))))
     ;; Change a group directive and reload
     ;;
     (let ((menu (cdr (gethash 'lisp-interaction-mode yas--menu-table))))
       (should (find "barbar" menu :key #'third :test #'string=)))

     (with-temp-buffer
       (insert "# group: foofoo\n# --\n(ert-deftest ${1:name} () $0)")
       (write-region nil nil (concat (first (yas-snippet-dirs))
                                     "/lisp-interaction-mode/ert-deftest")))
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'lisp-interaction-mode yas--menu-table))))
       (should (eql 1 (length menu)))
       (should (find "foofoo" menu :key #'third :test #'string=))
       (should (keymapp
                (fourth
                 (find "foofoo" menu :key #'third :test #'string=))))))))


;;; Helpers
;;;

(defun yas-should-expand (keys-and-expansions)
  (dolist (key-and-expansion keys-and-expansions)
    (yas-exit-all-snippets)
    (erase-buffer)
    (insert (car key-and-expansion))
    (let ((yas-fallback-behavior nil))
      (ert-simulate-command '(yas-expand)))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       (cdr key-and-expansion))))
  (yas-exit-all-snippets))

(defun yas-should-not-expand (keys)
  (dolist (key keys)
    (yas-exit-all-snippets)
    (erase-buffer)
    (insert key)
    (let ((yas-fallback-behavior nil))
      (ert-simulate-command '(yas-expand)))
    (should (string= (buffer-substring-no-properties (point-min) (point-max)) key))))

(defun yas-mock-insert (string)
  (interactive)
  (do ((i 0 (1+ i)))
      ((= i (length string)))
    (insert (aref string i))))

(defun yas-make-file-or-dirs (ass)
  (let ((file-or-dir-name (car ass))
        (content (cdr ass)))
    (cond ((listp content)
           (make-directory file-or-dir-name 'parents)
           (let ((default-directory (concat default-directory "/" file-or-dir-name)))
             (mapc #'yas-make-file-or-dirs content)))
          ((stringp content)
           (with-temp-buffer
             (insert content)
             (write-region nil nil file-or-dir-name nil 'nomessage)))
          (t
           (message "[yas] oops don't know this content")))))


(defun yas-variables ()
  (let ((syms))
    (mapatoms #'(lambda (sym)
                  (if (and (string-match "^yas-[^/]" (symbol-name sym))
                           (boundp sym))
                      (push sym syms))))
    syms))

(defun yas-call-with-saving-variables (fn)
  (let* ((vars (yas-variables))
         (saved-values (mapcar #'symbol-value vars)))
    (unwind-protect
        (funcall fn)
      (loop for var in vars
            for saved in saved-values
            do (set var saved)))))

(defmacro yas-saving-variables (&rest body)
  `(yas-call-with-saving-variables #'(lambda () ,@body)))


(defun yas-call-with-snippet-dirs (dirs fn)
  (let* ((default-directory (make-temp-file "yasnippet-fixture" t))
         (yas-snippet-dirs (mapcar #'car dirs)))
    (with-temp-message ""
      (unwind-protect
          (progn
            (mapc #'yas-make-file-or-dirs dirs)
            (funcall fn))
        (when (>= emacs-major-version 23)
          (delete-directory default-directory 'recursive))))))

(defmacro yas-with-snippet-dirs (dirs &rest body)
  `(yas-call-with-snippet-dirs ,dirs
                               #'(lambda ()
                                   ,@body)))

;;; Older emacsen
;;;
(unless (fboundp 'special-mode)
  (define-minor-mode special-mode "Just a placeholder for something isn't in emacs 22"))

;;; btw to test this in emacs22 mac osx:
;;; curl -L -O https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert.el
;;; curl -L -O https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert-x.el
;;; /usr/bin/emacs -nw -Q -L . -l yasnippet-tests.el --batch -e ert


(provide 'yasnippet-tests)
;;; yasnippet-tests.el ends here
;; Local Variables:
;; lexical-binding: t
;; End:
