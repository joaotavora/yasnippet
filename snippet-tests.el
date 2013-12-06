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
        (sprintf-maybe ((&mirror 0 (unless field-empty-p "s"))
                        "printf ("
                        (&field 0)
                        (&mirror 0 (unless field-empty-p ","))
                        "\""
                        (&field 1 "%s")
                        (&mirror 1 (if (string-match "%" field-string) "\"," "\")"))
                        (&field 2)
                        (&mirror 1 (if (string-match "%" field-string) "\)" ""))))
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

(ert-deftest more-nesting ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'more-nesting)
    (should (equal (buffer-string) "a 'rainniar' and a field: 'rainniar'"))
    (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
    (should (equal (buffer-string) "a bar and a field: bar"))
    (ert-simulate-command '(snippet-next-field))
    (ert-simulate-command '((lambda () (interactive) (insert "baz"))))
    (should (equal (buffer-string) "a barbaz bar"))
    (ert-simulate-command '(snippet-prev-field))
    (ert-simulate-command '((lambda () (interactive) (insert "foo"))))
    (should (equal (buffer-string) "a foobarbaz foobar"))))

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

(ert-deftest constants-and-default-values ()
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

(ert-deftest wrap-selected-region ()
  ;; this test needs some work. testing with `region-active-p' is hard
  ;; and also the "delete-selection" behaviour isn't decided yet
  ;;
  :expected-result :failed
  (with-temp-buffer
    (ert-simulate-command '((lambda () (interactive) (insert "bar"))))
    (set-mark (point))
    (goto-char (point-max))
    (snippet--insert-test-snippet 'wrap-selected-region)
    (should (equal (buffer-string)
                   "foobarbazbar"))))

(ert-deftest navigate-fields-and-exit ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'navigate-fields)
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
    (should (null (overlay-buffer snippet--field-overlay)))
    (should (looking-at "barbazquuxquam"))))


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


;;; Snippet parser tests.
;;;

(ert-deftest parse-string-literals ()
  (should (equal (snippet--parse-snippet "") '("")))
  (should (equal (snippet--parse-snippet "foobar") '("foobar"))))

(ert-deftest parse-escape-sequences ()
  (should (equal (snippet--parse-snippet "foobar\\$123") '("foobar$123")))
  (should (equal (snippet--parse-snippet "foobar\\\\") '("foobar\\")))
  (should (equal (snippet--parse-snippet "foobar\\\\\\\\") '("foobar\\\\")))
  (should (equal (snippet--parse-snippet "\\$") '("$")))
  (should (equal (snippet--parse-snippet "\\a") '("a"))))

(ert-deftest parse-invalid-escape-sequences ()
  :expected-result :failed
  (should-error (snippet--parse-snippet "\\"))
  (should-error (snippet--parse-snippet "foobar \\"))
  (should-error (snippet--parse-snippet "foobar \\\\\\")))

(ert-deftest parse-eval-blocks ()
  (should (equal (snippet--parse-snippet "foo`(upcase region-string)`bar")
                 '("foo" (upcase region-string) "bar")))
  (should (equal (snippet--parse-snippet "`(upcase region-string)`")
                 '((upcase region-string))))
  (should (equal (snippet--parse-snippet "`(apply concat \\`(,region-string \"foobar\"))`")
                 '((apply concat `(,region-string "foobar"))))))


(ert-deftest parse-tabstops ()
  (should (equal (snippet--parse-snippet "foo$1")
                 '("foo" (&field "1" nil))))

  (should (equal (snippet--parse-snippet "foo$123")
                 '("foo" (&field "123" nil))))

  (should (equal (snippet--parse-snippet "foo$1 $2 $1")
                 '("foo" (&field "1" nil) " " (&field "2" nil) " "
                   (&mirror "1" nil)))))

(ert-deftest parse-exits ()
  (should (equal (snippet--parse-snippet "$0") '((&exit nil))))
  (should (equal (snippet--parse-snippet "${0:foobar}") '((&exit "foobar"))))
  (should (equal (snippet--parse-snippet "${0:`(upcase \"foobar\")`}")
                 '((&exit (upcase "foobar"))))))

(ert-deftest parse-primary-field ()
  :expected-result :failed
  (should (equal (snippet--parse-snippet "$1 ${1:foobar} $1")
                 '((&mirror "1" nil) " " (&field "1" "foobar") " "
                   (&mirror "1" nil))))

  (should (equal (snippet--parse-snippet "${1:$(upcase region-string)} $1")
                 '((&mirror "1" (&transform (upcase region-string))) " "
                   (&field "1" nil))))

  (should (equal (snippet--parse-snippet "${1:$(upcase region-string)} $1 ${1:foobar}")
                 '((&mirror "1" (&transform (upcase region-string))) " "
                   (&mirror "1" nil) " " (&field "1" "foobar")))))

(ert-deftest parse-field-contents ()
  (should (equal (snippet--parse-snippet "${1:foo`(upcase region-string)`bar}")
                 '((&field "1" (&eval (concat ("foo"
                                               (upcase region-string)
                                               "bar")))))))

  (should (equal (snippet--parse-snippet "${1:foo$2bar}")
                 '((&field "1" (&nested "foo" (&field "2" nil) "bar"))))))
