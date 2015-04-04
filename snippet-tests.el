;;; snippet-tests.el --- some basic tests for snippet.el -*- lexical-binding: t; -*-

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
        (nested
         ;; static
         ;;
         ("a "
          (&field 1 (&nested (&field 2 "nested")
                             " "
                             (&field 3 "field")))
          " and its mirror: "
          (&mirror 1))
         ;; dynamic
         ((insert "a ")
          (&field 1
            (&field 2 (insert "nested"))
            (insert " ")
            (&field 3 (insert "field")))
          (insert " and its mirror: ")
          (&mirror 1 (s e)
            (insert s))))
        (mirror-of-nested-field ("a "
                                 (&field 1 (&nested (&field 2 "nested")
                                                    " "
                                                    (&field 3 "field")))
                                 (&mirror 3 (concat ", nested mirroring: "
                                                    field-string))))
        (more-nesting ("a "
                       (&field 1 (&nested
                                  "'"
                                  (&field 2 "rain")
                                  (&mirror 2 (apply #'string
                                                    (reverse
                                                     (string-to-list
                                                      field-string))))
                                  "'"))
                       (&field 3 " and a field:")
                       " "
                       (&mirror 1)))
        (printf ("printf (\""
                 (&field 1 "%s")
                 (&mirror 1 (if (string-match "%" field-string) "\"," "\")"))
                 (&field 2)
                 (&mirror 1 (if (string-match "%" field-string) "\)" ""))))
        (sprintf-maybe
         ;; static version
         ;; 
         ((&mirror 0 (unless field-empty-p "s"))
          "printf ("
          (&field 0)
          (&mirror 0 (unless field-empty-p ","))
          "\""
          (&field 1 "%s")
          (&mirror 1 (if (string-match "%" field-string) "\"," "\")"))
          (&field 2)
          (&mirror 1 (if (string-match "%" field-string) "\)" "")))
         ;; dynamic version
         ;; 
         ((&mirror 0 (_field-string field-empty-p)
            (unless field-empty-p "s"))
          (insert "printf (")
          (&field 0)
          (&mirror 0 (_field-string field-empty-p)
            (unless field-empty-p ","))
          (insert "\"")
          (&field 1 (insert "%s"))
          (&mirror 1
            (field-string _field-empty-p)
            (if (string-match "%" field-string) "\"," "\")"))
          (&field 2)
          (&mirror 1
            (field-string _field-empty-p)
            (if (string-match "%" field-string) "\)" ""))))
        (emacs-version ((&field 1 emacs-version)
                        " " (upcase (emacs-version)) " "
                        (&mirror 1)))
        (wrap-selected-region ("foo"
                               selected-text
                               "baz"
                               (&field 1 selected-text)))
        (navigate-fields ("foo"
                          &exit
                          (&field 2)
                          &field
                          (&field last)
                          (&field 1)))))

(defun snippet--get-fixture (name &optional dynamic-p)
  (let* ((assoc (assoc name snippet--test-snippets-alist)))
    (if dynamic-p
        (caddr assoc)
      (cadr assoc))))

(defmacro snippet--define-expansion-test (name fixture-name _args &rest body)
  (declare (indent 3))
  `(progn
     (ert-deftest ,(intern (concat (symbol-name name) "-static")) ()
       (let ((fixture (snippet--get-fixture ',fixture-name nil)))
         (if (not fixture)
             (ert-skip "No fixture for static test")
           (with-temp-buffer
             (eval `(with-static-snippet ,@fixture))
             ,@body))))
     (ert-deftest ,(intern (concat (symbol-name name) "-dynamic")) ()
       (let ((fixture (snippet--get-fixture ',fixture-name 'dynamic)))
         (if (not fixture)
             (ert-skip "No fixture for dynamic test")
           (with-temp-buffer
             (eval `(with-dynamic-snippet ,@fixture))
           ,@body))))))


(snippet--define-expansion-test basic-expansion basic ()
  (should (equal (buffer-string) "foo bar foo"))
  (should (equal (buffer-substring (overlay-start snippet--field-overlay)
                                   (overlay-end snippet--field-overlay))
                 "foo" )))

(snippet--define-expansion-test basic-clear-field basic ()
  (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
  (should (equal (buffer-string) "baz bar baz")))

(snippet--define-expansion-test basic-delete-char-in-field basic ()
  (ert-simulate-command '(delete-forward-char 1))
  (ert-simulate-command '((lambda () (interactive) (insert "b"))))
  (should (equal (buffer-string) "boo bar boo")))

(snippet--define-expansion-test contrived contrived ()
  (should (equal (buffer-string) ""))
  (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
  (should (equal (buffer-string) "foobarbaz")))

(snippet--define-expansion-test contrived-2 contrived ()
  (should (equal (buffer-string) ""))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
  (ert-simulate-command '(snippet-prev-field))
  (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
  (ert-simulate-command '(snippet-prev-field))
  (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
  (should (equal (buffer-string) "foobarbaz")))

(snippet--define-expansion-test nested-expansion nested ()
  (should (equal (buffer-string) "a nested field and its mirror: nested field"))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "nicely"))))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "nested field"))))
  (should (equal (buffer-substring (overlay-start snippet--field-overlay)
                                   (overlay-end snippet--field-overlay))
                 "nested field" ))
  (should (equal (buffer-string) "a nicely nested field and its mirror: nicely nested field")))

(snippet--define-expansion-test nested-skip-fields nested ()
  (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
  (should (equal (buffer-string) "a foo and its mirror: foo"))
  ;; this should exit the snippet now, since the two remaining
  ;; fields should be skipped
  (ert-simulate-command '(snippet-next-field))
  (should (null snippet--field-overlay)))

(snippet--define-expansion-test mirror-of-nested-field mirror-of-nested-field()
  (should (equal (buffer-string) "a nested field, nested mirroring: field"))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
  (should (equal (buffer-string) "a nested foo, nested mirroring: foo")))

(snippet--define-expansion-test more-nesting more-nesting()
  (should (equal (buffer-string) "a 'rainniar' and a field: 'rainniar'"))
  (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
  (should (equal (buffer-string) "a bar and a field: bar"))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
  (should (equal (buffer-string) "a barbaz bar"))
  (ert-simulate-command '(snippet-prev-field))
  (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
  (should (equal (buffer-string) "a foobarbaz foobar")))

(snippet--define-expansion-test printf-expansion printf ()
  (should (equal (buffer-string) "printf (\"%s\",)")))

(snippet--define-expansion-test printf-mirrors printf ()
  (ert-simulate-command '(delete-forward-char 1))
  (should (equal (buffer-string) "printf (\"s\")"))
  (ert-simulate-command '((lambda () (interactive) (insert "%"))))
  (should (equal (buffer-string) "printf (\"%s\",)")))

(snippet--define-expansion-test printf-mirrors-and-navigation printf ()
  (ert-simulate-command '(delete-forward-char 1))
  (should (equal (buffer-string) "printf (\"s\")"))
  (ert-simulate-command '((lambda () (interactive) (insert "%"))))
  (should (equal (buffer-string) "printf (\"%s\",)"))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "somevar"))))
  (should (equal (buffer-string) "printf (\"%s\",somevar)")))

(snippet--define-expansion-test printf-jump-to-second-field-right-away printf ()
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "somevar"))))
  (should (equal (buffer-string) "printf (\"%s\",somevar)")))

(snippet--define-expansion-test sprintf-variation sprintf-maybe()
  (should (equal (buffer-string) "printf (\"%s\",)"))
  (ert-simulate-command '((lambda () (interactive) (insert "somestring"))))
  (should (equal (buffer-string) "sprintf (somestring,\"%s\",)"))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '(snippet-next-field))
  (should (looking-back "sprintf (somestring,\"%s\","))
  (ert-simulate-command '(snippet-prev-field))
  (ert-simulate-command '((lambda () (interactive) (insert "bla"))))
  (should (equal (buffer-string) "sprintf (somestring,\"bla\")"))
  (should (looking-back "sprintf (somestring,\"bla"))
  (ert-simulate-command '(snippet-next-field))
  (should (looking-back "sprintf (somestring,\"bla\")")))

(snippet--define-expansion-test constants-and-default-values emacs-version()
  (should (equal (buffer-string)
                 (concat emacs-version " "
                         (upcase (emacs-version)) " "
                         emacs-version)))
  (ert-simulate-command '((lambda () (interactive) (insert "somestring"))))
  (should (equal (buffer-string)
                 (concat "somestring" " "
                         (upcase (emacs-version)) " "
                         "somestring"))))

(ert-deftest wrap-selected-region ()
  ;; this test needs some work. testing with `region-active-p' is hard
  ;; and also the "delete-selection" behaviour isn't decided yet
  ;;
  :expected-result :failed
  (with-temp-buffer
    (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
    (set-mark (point))
    (goto-char (point-max))
    (eval `(with-static-snippet ,@(snippet--get-fixture 'wrap-selected-region)))
    (should (equal (buffer-string)
                   "foobarbazbar"))))

(snippet--define-expansion-test navigate-fields-and-exit navigate-fields()
  (should (equal (buffer-string) "foo"))
  (ert-simulate-command '((lambda () (interactive) (insert "quam"))))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
  (ert-simulate-command '(snippet-next-field))
  (ert-simulate-command '((lambda () (interactive) (insert "quux"))))
  (ert-simulate-command '(snippet-next-field))
  (should (equal (buffer-string) "foobarbazquuxquam"))
  (should (null snippet--field-overlay))
  (should (looking-at "barbazquuxquam")))


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
                 '(&field 1 (&nested (&eval (foo)) (&eval (bar))))))
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

  ;; exit
  ;;
  (should (equal (snippet--canonicalize-form '&exit)
                 '(&exit (&eval nil))))
  (should (equal (snippet--canonicalize-form `(&exit))
                 '(&exit (&eval nil))))
  (should (equal (snippet--canonicalize-form `(&exit (foo)))
                 '(&exit (&eval (foo)))))
  ;; constants
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


;; misc
;; 
(cl-loop for sym in (list 'snippet--define-expansion-test)
         for regexp = (format "(\\(%S\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
                              sym)
         do (font-lock-add-keywords
             'emacs-lisp-mode
             `((,regexp (1 font-lock-keyword-face)
                        (2 font-lock-variable-name-face)))))

(defun snippet--test-fixture (fixture &optional dynamic)
  (with-current-buffer (get-buffer-create "*snippet-test*")
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (if dynamic
        (eval `(with-dynamic-snippet ,@(snippet--get-fixture fixture 'dynamic-p)))
      (eval `(with-static-snippet ,@(snippet--get-fixture fixture nil))))))

(provide 'snippet-tests)

