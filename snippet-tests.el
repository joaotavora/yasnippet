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
      `((basic ((field 1 "foo")
                " bar "
                (mirror 1)))
        (printf ("printf (\""
                 (field 1 "%s")
                 (mirror 1 (if (string-match "%" field-text) "\"," "\")"))
                 (field 2)
                 (mirror 1 (if (string-match "%" field-text) "\)" ""))))
        (sprintf-maybe ((mirror 0 (when field-text "s"))
                        "printf ("
                        (field 0)
                        (mirror 0 (when field-text ","))
                        "\""
                        (field 1 "%s")
                        (mirror 1 (if (string-match "%" field-text) "\"," "\")"))
                        (field 2)
                        (mirror 1 (if (string-match "%" field-text) "\)" ""))))))

(defun snippet--insert-test-snippet (name)
  (funcall (eval `(make-snippet ,(cadr (assoc name snippet--test-snippets-alist))))))


(ert-deftest foo-expansion ()
  (with-temp-buffer
    (snippet--insert-test-snippet 'basic)
    (should (equal (buffer-string) "foo bar foo"))
    (should (equal (buffer-substring (overlay-start snippet--field-overlay)
                                     (overlay-end snippet--field-overlay))
                   "foo" ))))

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
