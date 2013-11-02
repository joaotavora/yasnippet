;;; snippet-tests.el --- some basic tests for snippet.el

;; Copyright (C) 2013

;; Author: ;;; some basic test snippets <joaot@BELMONTE>
;; Keywords:

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

;;

;;; Code:

(require 'snippet)
(require 'ert)
(require 'ert-x)

(defvar snippet--test-snippets-alist nil)
(setq snippet--test-snippets-alist
      `((basic ((&field 1 "foo")
                " bar "
                (&mirror 1)))
        (contrived ((&field 1)
                    (&field 2)
                    (&field 3)))
        (nested ("a "
                 (&field 1 (&nested (&field 2 "nested")
                                    " "
                                    (&field 3 "field")))
                 " and its mirror: "
                 (&mirror 1)))
        (mirror-of-nested-field ("a "
                                 (&field 1 (&nested (&field 2 "nested")
                                                    " "
                                                    (&field 3 "field")))
                                 (&mirror 3 (concat ", nested mirroring: "
                                                    field-text))))
        (printf ("printf (\""
                 (&field 1 "%s")
                 (&mirror 1 (if (string-match "%" field-text) "\"," "\")"))
                 (&field 2)
                 (&mirror 1 (if (string-match "%" field-text) "\)" ""))))
        (sprintf-maybe ((&mirror 0 (when field-text "s"))
                        "printf ("
                        (&field 0)
                        (&mirror 0 (when field-text ","))
                        "\""
                        (&field 1 "%s")
                        (&mirror 1 (if (string-match "%" field-text) "\"," "\")"))
                        (&field 2)
                        (&mirror 1 (if (string-match "%" field-text) "\)" ""))))
        (emacs-version ((&field 1 emacs-version)
                        " " (upcase (emacs-version)) " "
                        (&mirror 1)))))

(defun snippet--insert-test-snippet (name)
  (funcall (make-snippet (cadr (assoc name snippet--test-snippets-alist)))))

(ert-deftest basic-expansion ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'basic)
    (should (equal (buffer-string) "foo bar foo"))
    (should (equal (buffer-substring (overlay-start snippet--field-overlay)
                                     (overlay-end snippet--field-overlay))
                   "foo" ))))

(ert-deftest basic-clear-field ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'basic)
    (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
    (should (equal (buffer-string) "baz bar baz"))))

(ert-deftest basic-delete-char-in-field ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'basic)
    (ert-simulate-command '(delete-forward-char 1))
    (ert-simulate-command '((lambda () (interactive) (insert "b"))))
    (should (equal (buffer-string) "boo bar boo"))))

(ert-deftest contrived ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'contrived)
    (should (equal (buffer-string) ""))
    (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
    (should (equal (buffer-string) "foobarbaz"))))

(ert-deftest contrived-2 ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'contrived)
    (should (equal (buffer-string) ""))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
    (ert-simulate-command '(snippet-prev-field))
    (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
    (ert-simulate-command '(snippet-prev-field))
    (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
    (should (equal (buffer-string) "foobarbaz"))))

(ert-deftest nested-expansion ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'nested)
    (should (equal (buffer-string) "a nested field and its mirror: nested field"))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "nicely"))))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "nested field"))))
    (should (equal (buffer-substring (overlay-start snippet--field-overlay)
                                     (overlay-end snippet--field-overlay))
                   "nested field" ))
    (should (equal (buffer-string) "a nicely nested field and its mirror: nicely nested field"))))

(ert-deftest nested-skip-fields ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'nested)
    (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
    (should (equal (buffer-string) "a foo and its mirror: foo"))
    ;; this should exit the snippet now, since the two remaining
    ;; fields should be skipped
    (ert-simulate-command '(snippet-next-field))
    (should (null (overlay-buffer snippet--field-overlay)))))

(ert-deftest mirror-of-nested-field ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'mirror-of-nested-field)
    (should (equal (buffer-string) "a nested field, nested mirroring: field"))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
    (should (equal (buffer-string) "a nested foo, nested mirroring: foo"))))

(ert-deftest printf-expansion ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'printf)
    (should (equal (buffer-string) "printf (\"%s\",)"))))

(ert-deftest printf-mirrors ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'printf)
    (ert-simulate-command '(delete-forward-char 1))
    (should (equal (buffer-string) "printf (\"s\")"))
    (ert-simulate-command '((lambda () (interactive) (insert "%"))))
    (should (equal (buffer-string) "printf (\"%s\",)"))))

(ert-deftest printf-mirrors-and-navigation ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'printf)
    (ert-simulate-command '(delete-forward-char 1))
    (should (equal (buffer-string) "printf (\"s\")"))
    (ert-simulate-command '((lambda () (interactive) (insert "%"))))
    (should (equal (buffer-string) "printf (\"%s\",)"))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "somevar"))))
    (should (equal (buffer-string) "printf (\"%s\",somevar)"))))

(ert-deftest printf-jump-to-second-field-right-away ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'printf)
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "somevar"))))
    (should (equal (buffer-string) "printf (\"%s\",somevar)"))))

(ert-deftest sprintf-variation ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'sprintf-maybe)
    (should (equal (buffer-string) "printf (\"%s\",)"))
    (ert-simulate-command '((lambda () (interactive) (insert "somestring"))))
    (should (equal (buffer-string) "sprintf (somestring,\"%s\",)"))))

(ert-deftest emacs-version ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'emacs-version)
    (should (equal (buffer-string)
                   (concat emacs-version " "
                           (upcase (emacs-version)) " "
                           emacs-version)))
    (ert-simulate-command '((lambda () (interactive) (insert "somestring"))))
    (should (equal (buffer-string)
                   (concat "somestring" " "
                           (upcase (emacs-version)) " "
                           "somestring")))))


;;; input validation
;;;
(ert-deftest valid-forms ()
  ;; fields
  ;;
  (should (equal (snippet--canonicalize-form '(&field 1 (foo)))
                 '(&field 1 (&eval (foo)))))
  (should (equal (snippet--canonicalize-form '(&field 1 (&eval (foo))))
                 '(&field 1 (&eval (foo)))))
  (should (equal (snippet--canonicalize-form '(&field 1 (&transform (foo))))
                 '(&field 1 (&transform (foo)))))
  (should (equal (snippet--canonicalize-form '(&field 1 (&nested (foo) (bar))))
                 '(&field 1 (&nested (foo) (bar)))))
  (should (equal (snippet--canonicalize-form '(&field 1))
                 '(&field 1 nil)))


  ;; mirrors
  ;;
  (should (equal (snippet--canonicalize-form '(&mirror 1))
                 '(&mirror 1 (&transform field-string))))
  (should (equal (snippet--canonicalize-form '(&mirror 1 (foo)))
                 '(&mirror 1 (&transform (foo)))))
  (should (equal (snippet--canonicalize-form '(&mirror 1 (&transform (foo))))
                 '(&mirror 1 (&transform (foo)))))
  ;; normal forms
  ;;
  (should (equal (snippet--canonicalize-form "bla")
                 '(&eval "bla")))
  (should (equal (snippet--canonicalize-form '(&eval "bla"))
                 '(&eval "bla")))
  (should (equal (snippet--canonicalize-form '(foo))
                 '(&eval (foo))))
  (should (equal (snippet--canonicalize-form '(&eval (foo)))
                 '(&eval (foo)))))

(ert-deftest invalid-forms ()
  ;; fields
  (should-error (snippet--canonicalize-form '(&field 1 (&transform (foo) (bar)))))
  (should-error (snippet--canonicalize-form '(&field 1 (&eval (foo) (bar)))))
  (should-error (snippet--canonicalize-form '(&mirror 1 (foo) (bar))))
  (should-error (snippet--canonicalize-form '(&field 1 (foo) (bar))))
  (should-error (snippet--canonicalize-form '(&eval (foo) (bar)))))
