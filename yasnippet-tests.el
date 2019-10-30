;;; yasnippet-tests.el --- some yasnippet tests  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2015, 2017-2018  Free Software Foundation, Inc.

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

;; To test this in emacs22 mac osx:
;; curl -L -O https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert.el
;; curl -L -O https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert-x.el
;; /usr/bin/emacs -nw -Q -L . -l yasnippet-tests.el --batch -e ert

;;; Code:

(require 'yasnippet)
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'org)


;;; Helper macros and function

(defmacro yas-with-snippet-dirs (dirs &rest body)
  (declare (indent defun) (debug t))
  `(yas-call-with-snippet-dirs
    ,dirs #'(lambda () ,@body)))

(defun yas-should-expand (keys-and-expansions)
  (dolist (key-and-expansion keys-and-expansions)
    (yas-exit-all-snippets)
    (erase-buffer)
    (insert (car key-and-expansion))
    (ert-simulate-command '(yas-expand))
    (unless (string= (yas--buffer-contents) (cdr key-and-expansion))
      (ert-fail (format "\"%s\" should have expanded to \"%s\" but got \"%s\""
                        (car key-and-expansion)
                        (cdr key-and-expansion)
                        (yas--buffer-contents)))))
  (yas-exit-all-snippets))

(defun yas--collect-menu-items (menu-keymap)
  (let ((yas--menu-items ()))
    (map-keymap (lambda (_binding definition)
                  (when (eq (car-safe definition) 'menu-item)
                    (push definition yas--menu-items)))
                menu-keymap)
    yas--menu-items))

(defun yas-should-not-expand (keys)
  (dolist (key keys)
    (yas-exit-all-snippets)
    (erase-buffer)
    (insert key)
    (ert-simulate-command '(yas-expand))
    (unless (string= (yas--buffer-contents) key)
      (ert-fail (format "\"%s\" should have stayed put, but instead expanded to \"%s\""
                        key
                        (yas--buffer-contents))))))

(defun yas-mock-insert (string)
  (dotimes (i (length string))
    (let ((last-command-event (aref string i)))
      (ert-simulate-command '(self-insert-command 1)))))

(defun yas-mock-yank (string)
  (let ((interprogram-paste-function (lambda () string)))
    (ert-simulate-command '(yank nil))))

(defun yas--key-binding (key)
  "Like `key-binding', but override `this-command-keys-vector'.
This lets `yas--maybe-expand-from-keymap-filter' work as expected."
  (cl-letf (((symbol-function 'this-command-keys-vector)
             (lambda () (cl-coerce key 'vector))))
    (key-binding key)))

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
      (cl-loop for var in vars
               for saved in saved-values
               do (set var saved)))))

(defun yas-call-with-snippet-dirs (dirs fn)
  (let* ((default-directory (make-temp-file "yasnippet-fixture" t))
         (yas-snippet-dirs (mapcar (lambda (d) (expand-file-name (car d))) dirs)))
    (with-temp-message ""
      (unwind-protect
          (progn
            (mapc #'yas-make-file-or-dirs dirs)
            (funcall fn))
        (when (>= emacs-major-version 24)
          (delete-directory default-directory 'recursive))))))

;;; Older emacsen
;;;
(unless (fboundp 'special-mode)
  ;; FIXME: Why provide this default definition here?!?
  (defalias 'special-mode 'fundamental))

(unless (fboundp 'string-suffix-p)
  ;; introduced in Emacs 24.4
  (defun string-suffix-p (suffix string &optional ignore-case)
    "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
    (let ((start-pos (- (length string) (length suffix))))
      (and (>= start-pos 0)
           (eq t (compare-strings suffix nil nil
                                  string start-pos nil ignore-case))))))


;;; Snippet mechanics

(defun yas--buffer-contents ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest field-navigation ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} from ${2:another} ${3:mother}")
    (should (string= (yas--buffer-contents)
                     "brother from another mother"))
    (should (looking-at "brother"))
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (should (looking-at "another"))
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (should (looking-at "mother"))
    (ert-simulate-command '(yas-prev-field))
    (should (looking-at "another"))
    (ert-simulate-command '(yas-prev-field))
    (should (looking-at "brother"))))

(ert-deftest simple-mirror ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} from another $1")
    (should (string= (yas--buffer-contents)
                     "brother from another brother"))
    (yas-mock-insert "bla")
    (should (string= (yas--buffer-contents)
                     "bla from another bla"))))

(ert-deftest mirror-with-transformation ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} from another ${1:$(upcase yas-text)}")
    (should (string= (yas--buffer-contents)
                     "brother from another BROTHER"))
    (yas-mock-insert "bla")
    (should (string= (yas--buffer-contents)
                     "bla from another BLA"))))

(ert-deftest yas-mirror-many-fields ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:brother} and ${2:brother} are${1:$(if (string= (yas-field-value 1) (yas-field-value 2)) \" \" \" not \")}the same word")
    (should (string= (yas--buffer-contents)
                     "brother and brother are the same word"))
    (yas-mock-insert "bla")
    (should (string= (yas--buffer-contents)
                     "bla and brother are not the same word"))
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (yas-mock-insert "bla")
    (should (string= (yas--buffer-contents)
                     "bla and bla are the same word"))))

(ert-deftest mirror-with-transformation-and-autofill ()
  "Test interaction of autofill with mirror transforms"
  (let ((words "one two three four five")
        filled-words)
    (with-temp-buffer
      (c-mode)      ; In `c-mode' filling comments works by narrowing.
      (yas-minor-mode +1)
      (setq fill-column 10)
      (auto-fill-mode +1)
      (yas-expand-snippet "/* $0\n */")
      (yas-mock-insert words)
      (setq filled-words (delete-and-extract-region (point-min) (point-max)))
      (yas-expand-snippet "/* $1\n */\n$2$2")
      (should (string= (yas--buffer-contents)
                       "/* \n */\n"))
      (yas-mock-insert words)
      (should (string= (yas--buffer-contents)
                       (concat filled-words "\n"))))))

(ert-deftest auto-fill-with-multiparagraph ()
  "Test auto-fill protection on snippet spanning multiple paragraphs"
  (with-temp-buffer
    (yas-minor-mode +1)
    (auto-fill-mode +1)
    (yas-expand-snippet "foo$1\n\n$2bar")
    (yas-mock-insert " ")
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (should (looking-at "bar"))))

(ert-deftest primary-field-transformation ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((snippet "${1:$$(upcase yas-text)}${1:$(concat \"bar\" yas-text)}"))
      (yas-expand-snippet snippet)
      (should (string= (yas--buffer-contents) "bar"))
      (yas-mock-insert "foo")
      (should (string= (yas--buffer-contents) "FOObarFOO")))))

(ert-deftest nested-placeholders-kill-superfield ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "brother from ${2:another ${3:mother}}!")
    (should (string= (yas--buffer-contents)
                     "brother from another mother!"))
    (yas-mock-insert "bla")
    (should (string= (yas--buffer-contents)
                     "brother from bla!"))))

(ert-deftest nested-placeholders-use-subfield ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "brother from ${2:another ${3:mother}}!")
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (yas-mock-insert "bla")
    (should (string= (yas--buffer-contents)
                     "brother from another bla!"))))

(ert-deftest mirrors-adjacent-to-fields-with-nested-mirrors ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "<%= f.submit \"${1:Submit}\"${2:$(and (yas-text) \", :disable_with => '\")}${2:$1ing...}${2:$(and (yas-text) \"'\")} %>")
    (should (string= (yas--buffer-contents)
                     "<%= f.submit \"Submit\", :disable_with => 'Submiting...' %>"))
    (yas-mock-insert "Send")
    (should (string= (yas--buffer-contents)
                     "<%= f.submit \"Send\", :disable_with => 'Sending...' %>"))))

(ert-deftest deep-nested-mirroring-issue-351 ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:FOOOOOOO}${2:$1}${3:$2}${4:$3}")
    (yas-mock-insert "abc")
    (should (string= (yas--buffer-contents) "abcabcabcabc"))))

(ert-deftest delete-numberless-inner-snippet-issue-562 ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${3:${test}bla}$0${2:ble}")
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (should (looking-at "testblable"))
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (ert-simulate-command '(yas-skip-and-clear-field))
    (should (looking-at "ble"))
    (should (null (yas-active-snippets)))))

(ert-deftest delete-nested-simple-field-issue-824 ()
  "Test deleting a field with a nested simple field in it."
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${3:so-$4and}$0${2:-so}")
    (ert-simulate-command '(yas-next-field-or-maybe-expand))
    (should (looking-at "so-and-so"))
    (ert-simulate-command '(yas-skip-and-clear-or-delete-char))
    (should (looking-at "-so"))
    (should (null (yas-active-snippets)))))

(ert-deftest ignore-trailing-whitespace ()
  (should (equal
           (with-temp-buffer
             (insert "# key: foo\n# --\nfoo")
             (yas--parse-template))
           (with-temp-buffer
             (insert "# key: foo \n# --\nfoo")
             (yas--parse-template)))))

;; (ert-deftest in-snippet-undo ()
;;   (with-temp-buffer
;;     (yas-minor-mode 1)
;;     (yas-expand-snippet "brother from ${2:another ${3:mother}}!")
;;     (ert-simulate-command '(yas-next-field-or-maybe-expand))
;;     (yas-mock-insert "bla")
;;     (ert-simulate-command '(undo))
;;     (should (string= (yas--buffer-contents)
;;                      "brother from another mother!"))))

(ert-deftest undo-redo ()
  "Check redoing of snippet undo."
  (yas-with-snippet-dirs '((".emacs.d/snippets"
                            ("emacs-lisp-mode" ("x" . "${1:one},and done"))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (yas-reload-all)
      (yas-minor-mode 1)
      (yas-expand-snippet "x$0")
      (let ((pre-expand-string (buffer-string)))
        (setq buffer-undo-list nil)
        (ert-simulate-command '(yas-expand))
        (push nil buffer-undo-list)
        (ert-simulate-command '(yas-next-field)) ; $1 -> exit snippet.
        (should (string-match-p "\\`one,and done" (buffer-string)))
        (push nil buffer-undo-list)
        (ert-simulate-command '(undo))  ; Revive snippet.
        (ert-simulate-command '(undo))  ; Undo expansion.
        (should (string= (buffer-string) pre-expand-string))
        (ert-simulate-command '(move-end-of-line 1))
        (push nil buffer-undo-list)
        (ert-simulate-command '(undo))  ; Redo (re-expand snippet).
        (should (string-match-p "\\`one,and done" (buffer-string)))))))

(ert-deftest undo-revive-and-do-again ()
  "Check undo-revived snippet is properly ended."
  ;; See https://github.com/joaotavora/yasnippet/issues/1006.
  (yas-with-snippet-dirs '((".emacs.d/snippets"
                            ("emacs-lisp-mode" ("x" . "${1:one},and done"))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (yas-reload-all)
      (yas-minor-mode 1)
      (yas-expand-snippet "x$0")
      (setq buffer-undo-list nil)
      (ert-simulate-command '(yas-expand))
      (push nil buffer-undo-list)
      (ert-simulate-command '(yas-next-field)) ; $1 -> exit snippet.
      (should (string-match-p "\\`one,and done" (buffer-string)))
      (push nil buffer-undo-list)
      (ert-simulate-command '(undo))    ; Revive snippet.
      (yas-mock-insert "abc")
      (ert-simulate-command '(yas-next-field)) ; $1 -> exit snippet again.
      (should (string-match-p "\\`abc,and done" (buffer-string)))
      ;; We should have exited snippet and cleaned up any overlays.
      (should-not (cl-some (lambda (o) (overlay-get o 'yas--snippet))
                           (overlays-in (point-min) (point-max)))))))


(defun yas-test-expand-and-undo (mode snippet-entry initial-contents)
  (yas-with-snippet-dirs
   `((".emacs.d/snippets" (,(symbol-name mode) ,snippet-entry)))
   (with-temp-buffer
     (funcall mode)
     (yas-reload-all)
     (yas-minor-mode 1)
     (yas-expand-snippet initial-contents)
     (let ((pre-expand-string (buffer-string)))
       (setq buffer-undo-list ())
       (ert-simulate-command '(yas-expand))
       ;; Need undo barrier, I think command loop puts it normally.
       (push nil buffer-undo-list)
       (ert-simulate-command '(undo))
       (should (string= (buffer-string) pre-expand-string))))))

(ert-deftest undo-indentation-1 ()
  "Check undoing works when only line of snippet is indented."
  (let ((yas-also-auto-indent-first-line t))
    (yas-test-expand-and-undo
     'emacs-lisp-mode '("s" . "(setq $0)") "(let\n(while s$0")))

(ert-deftest undo-indentation-2 ()
  "Check undoing works when only line of snippet is indented."
  (let ((yas-also-auto-indent-first-line t)
        (indent-tabs-mode nil))
    (yas-test-expand-and-undo
     'emacs-lisp-mode '("t" . "; TODO") "t$0")))

(ert-deftest undo-indentation-multiline-1 ()
  "Check undoing works when 1st line of multi-line snippet is indented."
  (let ((yas-also-auto-indent-first-line t)
        (indent-tabs-mode nil))
    (yas-test-expand-and-undo
     'js-mode '("if" . "if ($1) {\n\n}\n")
     "if$0\nabc = 123456789 + abcdef;")))


(ert-deftest undo-indentation-multiline-2 ()
  "Check undoing works when 2nd line of multi-line snippet is indented."
  (let ((yas-also-auto-indent-first-line t)
        (indent-tabs-mode nil))
    (yas-test-expand-and-undo
     'js-mode '("if" . "if (true) {\n${1:foo};\n}\n")
     "if$0\nabc = 123456789 + abcdef;")))

(ert-deftest dont-clear-on-partial-deletion-issue-515 ()
  "Ensure fields are not cleared when user doesn't really mean to."
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "my ${1:kid brother} from another ${2:mother}")

    (ert-simulate-command '(kill-word 1))
    (ert-simulate-command '(delete-char 1))

    (should (string= (yas--buffer-contents)
                     "my brother from another mother"))
    (should (looking-at "brother"))

    (ert-simulate-command '(yas-next-field))
    (should (looking-at "mother"))
    (ert-simulate-command '(yas-prev-field))
    (should (looking-at "brother"))))

(ert-deftest do-clear-on-yank-issue-515 ()
  "A yank should clear an unmodified field."
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "my ${1:kid brother} from another ${2:mother}")
    (yas-mock-yank "little sibling")
    (should (string= (yas--buffer-contents)
                     "my little sibling from another mother"))
    (ert-simulate-command '(yas-next-field))
    (ert-simulate-command '(yas-prev-field))
    (should (looking-at "little sibling"))))

(ert-deftest basic-indentation ()
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas-indent-line) 'auto)
    (set (make-local-variable 'yas-also-auto-indent-first-line) t)
    (yas-expand-snippet "def ${1:method}${2:(${3:args})}\n$0\nend")
    ;; Note that empty line is not indented.
    (should (string= "def method(args)

end" (buffer-string)))
    (cl-loop repeat 3 do (ert-simulate-command '(yas-next-field)))
    (yas-mock-insert (make-string (random 5) ?\ )) ; purposedly mess up indentation
    (yas-expand-snippet "class << ${self}\n  $0\nend")
    (ert-simulate-command '(yas-next-field))
    (should (string= "def method(args)
  class << self
    
  end
end" (buffer-string)))
    (should (= 4 (current-column)))))

(ert-deftest yas-also-indent-empty-lines ()
  "Respect `yas-also-indent-empty-lines' setting."
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas-indent-line) 'auto)
    (set (make-local-variable 'yas-also-auto-indent-first-line) t)
    (set (make-local-variable 'yas-also-indent-empty-lines) t)
    (yas-expand-snippet "def foo\n\nend")
    (should (string= "def foo\n  \nend" (buffer-string)))
    ;; Test that it keeps working without setting
    ;; `yas-also-auto-indent-first-line'.
    (setq yas-also-auto-indent-first-line nil)
    (erase-buffer)
    (yas-expand-snippet "def foo\n\nend")
    (should (string= "def foo\n  \nend" (buffer-string)))))

(ert-deftest yas-indent-first-line ()
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas-indent-line) 'auto)
    (set (make-local-variable 'yas-also-auto-indent-first-line) nil)
    (set (make-local-variable 'yas-also-indent-empty-lines) nil)
    (yas-expand-snippet "def foo\n$0\nend\n")
    ;; First (and only) line should not indent.
    (yas-expand-snippet "#not indented")
    (should (equal "def foo\n#not indented\nend\n" (buffer-string)))))

(ert-deftest yas-indent-first-line-fixed ()
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas-indent-line) 'fixed)
    (set (make-local-variable 'yas-also-auto-indent-first-line) nil)
    (set (make-local-variable 'yas-also-indent-empty-lines) nil)
    (yas-expand-snippet "    def foo\n    $0\n    end\n")
    ;; First (and only) line should not indent.
    (yas-expand-snippet "#not more indented")
    (should (equal "    def foo\n    #not more indented\n    end\n" (buffer-string)))))

(ert-deftest indentation-markers ()
  "Test a snippet with indentation markers (`$<')."
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas-indent-line) nil)
    (yas-expand-snippet "def ${1:method}${2:(${3:args})}\n$>Indent\nNo indent\\$>\nend")
    (should (string= "def method(args)
  Indent
No indent$>
end" (buffer-string)))))

(ert-deftest single-line-multi-mirror-indentation ()
  "Make sure not to indent with multiple mirrors per line."
  ;; See also Github issue #712.
  (with-temp-buffer
    (text-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:XXXXX} --------
$1   ---------------- $1 ----
$1   ------------------------")
    (should (string= (yas--buffer-contents) "XXXXX --------
XXXXX   ---------------- XXXXX ----
XXXXX   ------------------------"))))

(ert-deftest single-line-multi-mirror-indentation-2 ()
  "Like `single-line-multi-mirror-indentation' but 2 mirrors interleaved."
  ;; See also Github issue #768.
  (with-temp-buffer
    (c-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:one} ${2:two};\n$1 $2_;\n$2 $1_;\n")
    (should (string= (yas--buffer-contents)
                     "one two;\none two_;\ntwo one_;\n"))))

(ert-deftest indent-org-property ()
  "Handling of `org-mode' property indentation, see `org-property-format'."
  ;; This is an interesting case because `org-indent-line' calls
  ;; `replace-match' for properties.
  (with-temp-buffer
    (org-mode)
    (yas-minor-mode +1)
    (yas-expand-snippet "* Test ${1:test}\n:PROPERTIES:\n:ID: $1-after\n:END:")
    (yas-mock-insert "foo bar")
    (ert-simulate-command '(yas-next-field))
    (goto-char (point-min))
    (let ((expected (with-temp-buffer
                      (insert (format (concat "* Test foo bar\n"
                                              "  " org-property-format "\n"
                                              "  " org-property-format "\n"
                                              "  " org-property-format)
                                      ":PROPERTIES:" ""
                                      ":ID:" "foo bar-after"
                                      ":END:" ""))
                      (delete-trailing-whitespace)
                      (buffer-string))))
      ;; Some org-mode versions leave trailing whitespace, some don't.
      (delete-trailing-whitespace)
      (should (equal expected (buffer-string))))))

(ert-deftest indent-cc-mode ()
  "Handling of cc-mode's indentation."
  ;; This is an interesting case because cc-mode deletes all the
  ;; indentation before recreating it.
  (with-temp-buffer
    (c++-mode)
    (yas-minor-mode +1)
    (yas-expand-snippet "\
int foo()
{
    if ($1) {
        delete $1;
        $1 = 0;
    }
}")
    (yas-mock-insert "var")
    (should (string= "\
int foo()
{
  if (var) {
    delete var;
    var = 0;
  }
}" (buffer-string)))))

(ert-deftest indent-cc-mode-2 ()
  "Handling of cc-mode's preprocessor indentation."
  (with-temp-buffer
    (c-mode)
    (yas-minor-mode +1)
    (yas-expand-snippet "\
#ifndef `\"FOO\"`
#define FOO
#endif
")
    (should (string= "\
#ifndef FOO
#define FOO
#endif
" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest indent-snippet-mode ()
  "Handling of snippet-mode indentation."
  ;; This is an interesting case because newlines match [[:space:]] in
  ;; snippet-mode.
  (with-temp-buffer
    (snippet-mode)
    (yas-minor-mode +1)
    (yas-expand-snippet "# -*- mode: snippet -*-\n# name: $1\n# key: $1\n# --\n")
    (yas-mock-insert "foo")
    (should (string= "# -*- mode: snippet -*-\n# name: foo\n# key: foo\n# --\n"
                     (buffer-string)))))

(ert-deftest indent-mirrors-on-update ()
  "Check that mirrors are always kept indented."
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet "def $1\n$1\nend")
    (yas-mock-insert "xxx")
    ;; Assuming 2 space indent.
    (should (string= "def xxx\n  xxx\nend" (buffer-string)))))

(defun yas-test-delete-and-insert-command (beg end new)
  "Simulate a completion command (similar to company-mode)."
  (interactive "r\ns")
  ;; Simulate a completion command (like what company-mode does)
  ;; which deletes the "xxx" and then replaces it with something
  ;; else.
  (delete-region beg end)
  (insert new))

(ert-deftest indent-mirrors-on-complex-update ()
  "Don't get messed up by command that deletes and then inserts."
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet "def foo\n  ${1:slice} = append($1)\nend")
    (yas-mock-insert "xxx")
    (ert-simulate-command `(yas-test-delete-and-insert-command
                            ,(- (point) 3) ,(point) ,"yyy"))
    ;; Assuming 2 space indent.
    (should (string= "def foo\n  yyy = append(yyy)\nend" (buffer-string)))))



(ert-deftest snippet-with-multiline-mirrors-issue-665 ()
  "In issue 665, a multi-line mirror is attempted."
  (with-temp-buffer
    (ruby-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet "def initialize(${1:params})\n$2${1:$(
mapconcat #'(lambda (arg)
                 (format \"@%s = %s\" arg arg))
             (split-string yas-text \", \")
             \"\n\")}\nend")
    (yas-mock-insert "bla, ble, bli")
    (ert-simulate-command '(yas-next-field))
    (let ((expected (mapconcat #'identity
                               '("@bla = bla"
                                 ;; assume ruby is always indented to 2 spaces
                                 "  @ble = ble"
                                 "  @bli = bli")
                               "\n")))
      (should (looking-at expected))
      (yas-mock-insert "blo")
      (ert-simulate-command '(yas-prev-field))
      (ert-simulate-command '(yas-next-field))
      (should (looking-at (concat "blo" expected))))))

(defmacro yas-saving-variables (&rest body)
  (declare (debug t))
  `(yas-call-with-saving-variables #'(lambda () ,@body)))

(ert-deftest auto-next-field ()
  "Automatically exit a field after evaluating its transform."
  (with-temp-buffer
    (yas-saving-variables
     (yas-with-snippet-dirs
      `((".emacs.d/snippets"
         ("ruby-mode" ("snip" . ,(concat "foo ${1:$$"
                                         (prin1-to-string '(yas-auto-next
                                                            (yas-choose-value
                                                             "bar" "foo")))
                                         "} ${2:$$"
                                         (prin1-to-string '(yas-auto-next
                                                            (yas-choose-value
                                                             "too" "foo")))
                                         "} baz ${3:quinn} quinn")))))
      (yas-reload-all)
      (ruby-mode)
      (yas-minor-mode 1)
      (set (make-local-variable 'yas-prompt-functions) `(yas-no-prompt))
      (yas-mock-insert "snip")
      (ert-simulate-command '(yas-expand))
      (yas-mock-insert "quux")
      (should (equal "foo bar too baz quux quinn" (buffer-string)))))))


;;; Snippet expansion and character escaping
;;; Thanks to @zw963 (Billy) for the testing
;;;
(ert-deftest escape-dollar ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "bla\\${1:bla}ble")
    (should (string= (yas--buffer-contents) "bla${1:bla}ble"))))

(ert-deftest escape-closing-brace ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "bla${1:bla\\}}ble")
    (should (string= (yas--buffer-contents) "blabla}ble"))
    (should (string= (yas-field-value 1) "bla}"))))

(ert-deftest escape-backslashes ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "bla\\ble")
    (should (string= (yas--buffer-contents) "bla\\ble"))))

(ert-deftest escape-backquotes ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "bla`(upcase \"foo\\`bar\")`ble")
    (should (string= (yas--buffer-contents) "blaFOO`BARble"))))

(ert-deftest escape-some-elisp-with-strings ()
  "elisp with strings and unbalance parens inside it"
  (with-temp-buffer
    (yas-minor-mode 1)
    ;; The rules here is: to output a literal `"' you need to escape
    ;; it with one backslash. You don't need to escape them in
    ;; embedded elisp.
    (yas-expand-snippet "soon \\\"`(concat (upcase \"(my arms\")\"\\\" were all around her\")`")
    (should (string= (yas--buffer-contents) "soon \"(MY ARMS\" were all around her"))))

(ert-deftest escape-some-elisp-with-backslashes ()
  (with-temp-buffer
    (yas-minor-mode 1)
    ;; And the rule here is: to output a literal `\' inside a string
    ;; inside embedded elisp you need a total of six `\'
    (yas-expand-snippet "bla`(upcase \"hey\\\\\\yo\")`ble")
    (should (string= (yas--buffer-contents) "blaHEY\\YOble"))))

(ert-deftest be-careful-when-escaping-in-yas-selected-text ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((yas-selected-text "He\\\\o world!"))
      (yas-expand-snippet "Look ma! `(yas-selected-text)`")
      (should (string= (yas--buffer-contents) "Look ma! He\\\\o world!")))
    (yas-exit-all-snippets)
    (erase-buffer)
    (let ((yas-selected-text "He\"o world!"))
      (yas-expand-snippet "Look ma! `(yas-selected-text)`")
      (should (string= (yas--buffer-contents) "Look ma! He\"o world!")))
    (yas-exit-all-snippets)
    (erase-buffer)
    (let ((yas-selected-text "He\"\)\\o world!"))
      (yas-expand-snippet "Look ma! `(yas-selected-text)`")
      (should (string= (yas--buffer-contents) "Look ma! He\"\)\\o world!")))
    (yas-exit-all-snippets)
    (erase-buffer)))

(ert-deftest be-careful-when-escaping-in-yas-selected-text-2 ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((yas-selected-text "He)}o world!"))
      (yas-expand-snippet "Look ma! ${1:`(yas-selected-text)`} OK?")
      (should (string= (yas--buffer-contents) "Look ma! He)}o world! OK?")))))

(ert-deftest escaping-for-lsp-style-snippet-syntax ()
  "See Github #979."
  (should
   (string= (with-temp-buffer
              (yas-minor-mode 1)
              (yas-expand-snippet
               "Printf(${1:format string}, ${2:args ...interface{\\}})${0}")
              (yas--buffer-contents))
            (with-temp-buffer
              (yas-minor-mode 1)
              (yas-expand-snippet
               "Printf(${1:format string}, ${2:args ...interface\\{\\}})${0}")
              (yas--buffer-contents)))))

(ert-deftest insert-snippet-with-backslashes-in-active-field ()
  ;; This test case fails if `yas--inhibit-overlay-hooks' is not bound
  ;; in `yas-expand-snippet' (see Github #844).
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${1:$$(if (not yas-modified-p) \"a\")}")
    (yas-expand-snippet "\\\\alpha")))

(ert-deftest expand-with-unused-yas-selected-text ()
  (with-temp-buffer
    (yas-with-snippet-dirs
      '((".emacs.d/snippets"
         ("emacs-lisp-mode"
          ("foo" . "expanded `yas-selected-text`foo"))))
      (yas-reload-all)
      (emacs-lisp-mode)
      (yas-minor-mode +1)
      (insert "foo")
      (ert-simulate-command '(yas-expand))
      (should (equal (buffer-string) "expanded foo")))))

(ert-deftest yas-expand-command-snippet ()
  (with-temp-buffer
    (yas-with-snippet-dirs
      '((".emacs.d/snippets"
         ("emacs-lisp-mode"
          ("foo" . "\
# type: command
# --
\(insert \"expanded foo\")"))))
      (yas-reload-all)
      (emacs-lisp-mode)
      (yas-minor-mode +1)
      (insert "foo")
      (ert-simulate-command '(yas-expand))
      (should (equal (buffer-string) "expanded foo")))))

(ert-deftest example-for-issue-271 ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((yas-selected-text "aaa")
          (snippet "if ${1:condition}\n`yas-selected-text`\nelse\n$3\nend"))
      (yas-expand-snippet snippet)
      (yas-next-field)
      (yas-mock-insert "bbb")
      (should (string= (yas--buffer-contents) "if condition\naaa\nelse\nbbb\nend")))))

(ert-deftest yas-no-memory-of-bad-snippet ()
  "Expanding an incorrect snippet should not influence future expansions."
  ;; See https://github.com/joaotavora/yasnippet/issues/800.
  (with-temp-buffer
    (yas-minor-mode 1)
    (should-error (yas-expand-snippet "```foo\n\n```"))
    (erase-buffer) ; Bad snippet may leave wrong text.
    ;; But expanding the corrected snippet should work fine.
    (yas-expand-snippet "\\`\\`\\`foo\n\n\\`\\`\\`")
    (should (equal (buffer-string) "```foo\n\n```"))))

(defmacro yas--with-font-locked-temp-buffer (&rest body)
  "Like `with-temp-buffer', but ensure `font-lock-mode'."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    ;; NOTE: buffer name must not start with a space, otherwise
    ;; `font-lock-mode' doesn't turn on.
    `(let ((,temp-buffer (generate-new-buffer "*yas-temp*")))
       (with-current-buffer ,temp-buffer
         ;; pretend we're interactive so `font-lock-mode' turns on
         (let ((noninteractive nil)
               ;; turn on font locking after major mode change
               (change-major-mode-after-body-hook #'font-lock-mode))
           (unwind-protect
               (progn (require 'font-lock)
                      ;; turn on font locking before major mode change
                      (font-lock-mode +1)
                      ,@body)
             (and (buffer-name ,temp-buffer)
                  (kill-buffer ,temp-buffer))))))))

(ert-deftest example-for-issue-474 ()
  (yas--with-font-locked-temp-buffer
    (c-mode)
    (yas-minor-mode 1)
    (insert "#include <foo>\n")
    (let ((yas-good-grace nil)) (yas-expand-snippet "`\"TODO: \"`"))
    (should (string= (yas--buffer-contents) "#include <foo>\nTODO: "))))

(ert-deftest example-for-issue-404 ()
  (yas--with-font-locked-temp-buffer
    (c++-mode)
    (yas-minor-mode 1)
    (insert "#include <foo>\n")
    (let ((yas-good-grace nil)) (yas-expand-snippet "main"))
    (should (string= (yas--buffer-contents) "#include <foo>\nmain"))))

(ert-deftest example-for-issue-404-c-mode ()
  (yas--with-font-locked-temp-buffer
    (c-mode)
    (yas-minor-mode 1)
    (insert "#include <foo>\n")
    (let ((yas-good-grace nil)) (yas-expand-snippet "main"))
    (should (string= (yas--buffer-contents) "#include <foo>\nmain"))))

(ert-deftest middle-of-buffer-snippet-insertion ()
  (with-temp-buffer
    (yas-minor-mode 1)
    (insert "beginning")
    (save-excursion (insert "end"))
    (yas-expand-snippet "-middle-")
    (should (string= (yas--buffer-contents) "beginning-middle-end"))))

(ert-deftest another-example-for-issue-271 ()
  ;; expect this to fail in batch mode since `region-active-p' doesn't
  ;; used by `yas-expand-snippet' doesn't make sense in that context.
  ;;
  :expected-result (if noninteractive
                       :failed
                     :passed)
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((snippet "\\${${1:1}:`yas-selected-text`}"))
      (insert "aaabbbccc")
      (set-mark 4)
      (goto-char 7)
      (yas-expand-snippet snippet)
      (should (string= (yas--buffer-contents) "aaa${1:bbb}ccc")))))

(ert-deftest string-match-with-subregexp-in-embedded-elisp ()
  (with-temp-buffer
    (yas-minor-mode 1)
    ;; the rule here is: To use regexps in embedded `(elisp)` expressions, write
    ;; it like you would normal elisp, i.e. no need to escape the backslashes.
    (let ((snippet "`(if (string-match \"foo\\\\(ba+r\\\\)foo\" \"foobaaaaaaaaaarfoo\")
                         \"ok\"
                         \"fail\")`"))
      (yas-expand-snippet snippet))
    (should (string= (yas--buffer-contents) "ok"))))

(ert-deftest string-match-with-subregexp-in-mirror-transformations ()
  (with-temp-buffer
    (yas-minor-mode 1)
    ;; the rule here is: To use regexps in embedded `(elisp)` expressions,
    ;; escape backslashes once, i.e. to use \\( \\) constructs, write \\\\( \\\\).
    (let ((snippet "$1${1:$(if (string-match \"foo\\\\\\\\(ba+r\\\\\\\\)baz\" yas-text)
                                \"ok\"
                                \"fail\")}"))
      (yas-expand-snippet snippet)
      (should (string= (yas--buffer-contents) "fail"))
      (yas-mock-insert "foobaaar")
      (should (string= (yas--buffer-contents) "foobaaarfail"))
      (yas-mock-insert "baz")
      (should (string= (yas--buffer-contents) "foobaaarbazok")))))


;;; Misc tests
;;;
(ert-deftest protection-overlay-no-cheating ()
  "Protection overlays at the very end of the buffer are dealt
  with by cheatingly inserting a newline!

TODO: correct this bug!"
  :expected-result :failed
  (with-temp-buffer
    (yas-minor-mode 1)
    (yas-expand-snippet "${2:brother} from another ${1:mother}")
    (should (string= (yas--buffer-contents)
                     "brother from another mother") ;; no newline should be here!
            )))

(defvar yas-tests--ran-exit-hook nil)

(ert-deftest snippet-exit-hooks ()
  (with-temp-buffer
    (yas-saving-variables
     (let ((yas-tests--ran-exit-hook nil)
           (yas-triggers-in-field t))
       (yas-with-snippet-dirs
         '((".emacs.d/snippets"
            ("emacs-lisp-mode"
             ("foo" . "\
# expand-env: ((yas-after-exit-snippet-hook (lambda () (setq yas-tests--ran-exit-hook t))))
# --
FOO ${1:f1} ${2:f2}")
             ("sub" . "\
# expand-env: ((yas-after-exit-snippet-hook (lambda () (setq yas-tests--ran-exit-hook 'sub))))
# --
SUB"))))
         (yas-reload-all)
         (emacs-lisp-mode)
         (yas-minor-mode +1)
         (insert "foo")
         (ert-simulate-command '(yas-expand))
         (should-not yas-tests--ran-exit-hook)
         (yas-mock-insert "sub")
         (ert-simulate-command '(yas-expand))
         (ert-simulate-command '(yas-next-field))
         (should-not yas-tests--ran-exit-hook)
         (ert-simulate-command '(yas-next-field))
         (should (eq yas-tests--ran-exit-hook t)))))))

(ert-deftest snippet-exit-hooks-bindings ()
  "Check that `yas-after-exit-snippet-hook' is handled correctly
in the case of a buffer-local variable and being overwritten by
the expand-env field."
  (with-temp-buffer
    (yas-saving-variables
     (let ((yas-tests--ran-exit-hook nil)
           (yas-triggers-in-field t)
           (yas-after-exit-snippet-hook nil))
       (yas-with-snippet-dirs
         '((".emacs.d/snippets"
            ("emacs-lisp-mode"
             ("foo" . "foobar\n")
             ("baz" . "\
# expand-env: ((yas-after-exit-snippet-hook (lambda () (setq yas-tests--ran-exit-hook 'letenv))))
# --
foobaz\n"))))
         (yas-reload-all)
         (emacs-lisp-mode)
         (yas-minor-mode +1)
         (add-hook 'yas-after-exit-snippet-hook (lambda () (push 'global yas-tests--ran-exit-hook)))
         (add-hook 'yas-after-exit-snippet-hook (lambda () (push 'local yas-tests--ran-exit-hook)) nil t)
         (insert "baz")
         (ert-simulate-command '(yas-expand))
         (should (eq 'letenv yas-tests--ran-exit-hook))
         (insert "foo")
         (ert-simulate-command '(yas-expand))
         (should (eq 'global (nth 0 yas-tests--ran-exit-hook)))
         (should (eq 'local (nth 1 yas-tests--ran-exit-hook))))))))

(ert-deftest snippet-mirror-bindings ()
  "Check that variables defined with the expand-env field are
accessible from mirror transformations."
  (with-temp-buffer
    (yas-saving-variables
     (let ((yas-triggers-in-field t)
           (yas-good-grace nil))
       (yas-with-snippet-dirs
         '((".emacs.d/snippets"
            ("emacs-lisp-mode"
             ("baz" . "\
# expand-env: ((func #'upcase))
# --
hello ${1:$(when (stringp yas-text) (funcall func yas-text))} foo${1:$$(concat \"baz\")}$0"))))
         (yas-reload-all)
         (emacs-lisp-mode)
         (yas-minor-mode +1)
         (insert "baz")
         (ert-simulate-command '(yas-expand))
         (should (string= (yas--buffer-contents) "hello BAZ foobaz\n")))))))

(defvar yas--barbaz)
(defvar yas--foobarbaz)

;; See issue #497. To understand this test, follow the example of the
;; `yas-key-syntaxes' docstring.
;;
(ert-deftest complicated-yas-key-syntaxes ()
  (with-temp-buffer
    (yas-saving-variables
     (yas-with-snippet-dirs
       '((".emacs.d/snippets"
          ("emacs-lisp-mode"
           ("foo-barbaz" . "# condition: yas--foobarbaz\n# --\nOKfoo-barbazOK")
           ("barbaz" . "# condition: yas--barbaz\n# --\nOKbarbazOK")
           ("baz" . "OKbazOK")
           ("'quote" . "OKquoteOK"))))
       (yas-reload-all)
       (emacs-lisp-mode)
       (yas-minor-mode +1)
       (let ((yas-key-syntaxes '("w" "w_")))
         (let ((yas--barbaz t))
           (yas-should-expand '(("foo-barbaz" . "foo-OKbarbazOK")
                                ("barbaz" . "OKbarbazOK"))))
         (let ((yas--foobarbaz t))
           (yas-should-expand '(("foo-barbaz" . "OKfoo-barbazOK"))))
         (let ((yas-key-syntaxes
                (cons #'(lambda (_start-point)
                          (unless (eq ?- (char-before))
                            (backward-char)
                            'again))
                      yas-key-syntaxes))
               (yas--foobarbaz t))
           (yas-should-expand '(("foo-barbaz" . "foo-barOKbazOK")))))
       (let ((yas-key-syntaxes '(yas-try-key-from-whitespace)))
         (yas-should-expand '(("xxx\n'quote" . "xxx\nOKquoteOK")
                              ("xxx 'quote" . "xxx OKquoteOK"))))
       (let ((yas-key-syntaxes '(yas-shortest-key-until-whitespace))
             (yas--foobarbaz t) (yas--barbaz t))
         (yas-should-expand '(("foo-barbaz" . "foo-barOKbazOK")))
         (setq yas-key-syntaxes '(yas-longest-key-from-whitespace))
         (yas-should-expand '(("foo-barbaz" . "OKfoo-barbazOK")
                              ("foo " . "foo "))))))))

(ert-deftest nested-snippet-expansion-1 ()
  (with-temp-buffer
    (yas-minor-mode +1)
    (let ((yas-triggers-in-field t))
      (yas-expand-snippet "Parent $1 Snippet")
      (yas-expand-snippet "(Child $1 $2 Snippet)")
      (let ((snippets (yas-active-snippets)))
        (should (= (length snippets) 2))
        (should (= (length (yas--snippet-fields (nth 0 snippets))) 2))
        (should (= (length (yas--snippet-fields (nth 1 snippets))) 1))))))

(ert-deftest nested-snippet-expansion-2 ()
  (let ((yas-triggers-in-field t))
    (yas-with-snippet-dirs
      '((".emacs.d/snippets"
         ("text-mode"
          ("nest" . "one($1:$1) two($2).$0"))))
      (yas-reload-all)
      (text-mode)
      (yas-minor-mode +1)
      (insert "nest")
      (ert-simulate-command '(yas-expand))
      (yas-mock-insert "nest")
      (ert-simulate-command '(yas-expand))
      (yas-mock-insert "x")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (yas-mock-insert "y")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (yas-mock-insert "z")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (should (string= (buffer-string)
                       "one(one(x:x) two(y).:one(x:x) two(y).) two(z).")))))

(ert-deftest nested-snippet-expansion-3 ()
  (let ((yas-triggers-in-field t))
    (yas-with-snippet-dirs
      '((".emacs.d/snippets"
         ("text-mode"
          ("rt" . "\
\\sqrt${1:$(if (string-equal \"\" yas/text) \"\" \"[\")}${1:}${1:$(if (string-equal \"\" yas/text) \"\" \"]\")}{$2}$0"))))
      (yas-reload-all)
      (text-mode)
      (yas-minor-mode +1)
      (insert "rt")
      (ert-simulate-command '(yas-expand))
      (yas-mock-insert "3")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (yas-mock-insert "rt")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (yas-mock-insert "5")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (yas-mock-insert "2")
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (ert-simulate-command '(yas-next-field-or-maybe-expand))
      (should (string= (buffer-string) "\\sqrt[3]{\\sqrt[5]{2}}")))))

(ert-deftest nested-snippet-expansion-4 ()
  "See Github #959."
  (let ((yas-triggers-in-field t))
    (yas-with-snippet-dirs
     '((".emacs.d/snippets"
        ("text-mode"
         ("ch" . "<-${1:ch}"))))
     (yas-reload-all)
     (text-mode)
     (yas-minor-mode +1)
     (yas-expand-snippet "ch$0\n")
     (ert-simulate-command '(yas-expand))
     (ert-simulate-command '(forward-char 2))
     (ert-simulate-command '(yas-expand))
     (yas-mock-insert "abc")
     (ert-simulate-command '(yas-next-field-or-maybe-expand))
     (yas-mock-insert "def")
     (ert-simulate-command '(yas-next-field-or-maybe-expand))
     (should (string= (buffer-string) "<-<-abcdef\n")))))

(ert-deftest nested-snippet-expansion-5-nested-delete ()
  "See Github #996."
  (let ((yas-triggers-in-field t))
    (yas-with-snippet-dirs
     '((".emacs.d/snippets"
        ("text-mode"
         ("sel" . "${1:ch}")
         ("ch" . "<-${1:ch}"))))
     (yas-reload-all)
     (text-mode)
     (yas-minor-mode +1)
     (insert "sel")
     (ert-simulate-command '(yas-expand))
     (ert-simulate-command '(forward-word 1))
     (ert-simulate-command '(yas-expand))
     (ert-simulate-command '(forward-word 1))
     ;; The (cl-assert (memq pfield (yas--snippet-fields psnippet)))
     ;; in `yas--on-field-overlay-modification' failed here.
     (ert-simulate-command '(delete-backward-char 1))
     (should (string= (buffer-string) "<-c\n")))))


;;; Loading
;;;

(defmacro yas-with-overriden-buffer-list (&rest body)
  (declare (debug t))
  (let ((saved-sym (make-symbol "yas--buffer-list")))
    `(let ((,saved-sym (symbol-function 'buffer-list)))
       (cl-letf (((symbol-function 'buffer-list)
                  (lambda ()
                    (cl-remove-if (lambda (buf)
                                    (with-current-buffer buf
                                      (eq major-mode 'lisp-interaction-mode)))
                                  (funcall ,saved-sym)))))
         ,@body))))


(defmacro yas-with-some-interesting-snippet-dirs (&rest body)
  (declare (debug t))
  `(yas-saving-variables
    (yas-with-overriden-buffer-list
     (yas-with-snippet-dirs
       '((".emacs.d/snippets"
          ("c-mode"
           (".yas-parents" . "cc-mode")
           ("printf" . "printf($1);"))  ;; notice the overriding for issue #281
          ("emacs-lisp-mode" ("ert-deftest" . "(ert-deftest ${1:name} () $0)"))
          ("lisp-interaction-mode" (".yas-parents" . "emacs-lisp-mode")))
         ("library/snippets"
          ("c-mode"
           (".yas-parents" . "c++-mode")
           ("printf" . "printf"))
          ("cc-mode" ("def" . "# define"))
          ("emacs-lisp-mode" ("dolist" . "(dolist)"))
          ("lisp-interaction-mode" ("sc" . "brother from another mother"))))
       ,@body))))

(ert-deftest snippet-lookup ()
  "Test `yas-lookup-snippet'."
  (yas-with-some-interesting-snippet-dirs
   (yas-reload-all 'no-jit)
   (should (equal (yas--template-content (yas-lookup-snippet "printf" 'c-mode))
                  "printf($1);"))
   (should (equal (yas--template-content (yas-lookup-snippet "def" 'c-mode))
                  "# define"))
   (should-not (yas-lookup-snippet "no such snippet" nil 'noerror))
   (should-not (yas-lookup-snippet "printf" 'emacs-lisp-mode 'noerror))))

(ert-deftest yas-lookup-snippet-with-env ()
  (with-temp-buffer
    (yas-with-snippet-dirs
      '((".emacs.d/snippets"
         ("emacs-lisp-mode"
          ("foo" . "\
# expand-env: ((foo \"bar\"))
# --
`foo`"))))
      (yas-reload-all)
      (emacs-lisp-mode)
      (yas-minor-mode +1)
      (yas-expand-snippet (yas-lookup-snippet "foo"))
      (should (equal (buffer-string) "bar")))))

(ert-deftest basic-jit-loading ()
  "Test basic loading and expansion of snippets"
  (yas-with-some-interesting-snippet-dirs
   (yas-reload-all)
   (yas--basic-jit-loading-1)))

(ert-deftest basic-jit-loading-with-compiled-snippets ()
  "Test basic loading and expansion of compiled snippets"
  (yas-with-some-interesting-snippet-dirs
   (yas-reload-all)
   (yas-recompile-all)
   (cl-letf (((symbol-function 'yas--load-directory-2)
              (lambda (&rest _dummies)
                (ert-fail "yas--load-directory-2 shouldn't be called when snippets have been compiled"))))
     (yas-reload-all)
     (yas--basic-jit-loading-1))))

(ert-deftest snippet-load-uuid ()
  "Test snippets with same uuid override old ones."
  (yas-saving-variables
   (yas-define-snippets
    'text-mode
    '(("1" "one" "one" nil nil nil nil "C-c 1" "uuid-1")
      ("2" "two" "two" nil nil nil nil nil "uuid-2")))
   (with-temp-buffer
     (text-mode)
     (yas-minor-mode +1)
     (should (equal (yas--template-content (yas-lookup-snippet "one"))
                    "one"))
     (should (eq (yas--key-binding "\C-c1") 'yas-expand-from-keymap))
     (yas-define-snippets
      'text-mode '(("_1" "one!" "won" nil nil nil nil nil "uuid-1")))
     (should (null (yas-lookup-snippet "one" nil 'noerror)))
     (should (null (yas--key-binding "\C-c1")))
     (should (equal (yas--template-content(yas-lookup-snippet "won"))
                    "one!")))))

(ert-deftest snippet-save ()
  "Make sure snippets can be saved correctly."
  (yas-saving-variables
   (yas-with-snippet-dirs
    '((".emacs.d/snippets"
       ("text-mode")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'read-file-name)
               (lambda (_prompt &optional _dir _default _mustmatch initial _predicate)
                 (expand-file-name initial)))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (or (car collection) ""))))
      (with-temp-buffer
        (text-mode)
        (yas-minor-mode +1)
        (save-current-buffer
          (yas-new-snippet t)
          (with-current-buffer yas-new-snippet-buffer-name
            (snippet-mode)
            (insert "# name: foo\n# key: bar\n# --\nsnippet foo")
            (call-interactively 'yas-load-snippet-buffer-and-close)))
        (save-current-buffer
          (yas-new-snippet t)
          (with-current-buffer yas-new-snippet-buffer-name
            (snippet-mode)
            (insert "# name: bar\n# key: bar\n# --\nsnippet bar")
            (call-interactively 'yas-load-snippet-buffer-and-close)))
        (should (file-readable-p
                 (expand-file-name "foo" (car yas-snippet-dirs))))
        (should (file-readable-p
                 (expand-file-name "bar" (car yas-snippet-dirs)))))))))

(ert-deftest visiting-compiled-snippets ()
  "Test snippet visiting for compiled snippets."
  (yas-with-some-interesting-snippet-dirs
   (yas-recompile-all)
   (yas-reload-all 'no-jit) ; must be loaded for `yas-lookup-snippet' to work.
   (cl-letf (((symbol-function 'find-file-noselect)
              (lambda (filename &rest _)
                (throw 'yas-snippet-file filename))))
     (should (string-suffix-p
              "cc-mode/def"
              (catch 'yas-snippet-file
                (yas--visit-snippet-file-1
                 (yas--lookup-snippet-1 "def" 'cc-mode))))))))

(ert-deftest loading-with-cyclic-parenthood ()
  "Test loading when cyclic parenthood is setup."
  (yas-saving-variables
   (yas-with-snippet-dirs '((".emacs.d/snippets"
                             ("c-mode"
                              (".yas-parents" . "cc-mode"))
                             ("cc-mode"
                              (".yas-parents" . "yet-another-c-mode and-that-one"))
                             ("yet-another-c-mode"
                              (".yas-parents" . "c-mode and-also-this-one lisp-interaction-mode"))))
     (yas-reload-all)
     (with-temp-buffer
       (let* ((major-mode 'c-mode)
              (expected `(fundamental-mode
                          c-mode
                          cc-mode
                          yet-another-c-mode
                          and-also-this-one
                          and-that-one
                          ;; prog-mode doesn't exist in emacs 23.4
                          ,@(if (fboundp 'prog-mode)
                                '(prog-mode))
                          emacs-lisp-mode
                          lisp-interaction-mode))
              (observed (yas--modes-to-activate)))
         (should (equal major-mode (car observed)))
         (should (equal (sort expected #'string<) (sort observed #'string<))))))))

(ert-deftest extra-modes-parenthood ()
  "Test activation of parents of `yas--extra-modes'."
  (yas-saving-variables
   (yas-with-snippet-dirs '((".emacs.d/snippets"
                             ("c-mode"
                              (".yas-parents" . "cc-mode"))
                             ("yet-another-c-mode"
                              (".yas-parents" . "c-mode and-also-this-one lisp-interaction-mode"))))
     (yas-reload-all)
     (with-temp-buffer
       (yas-activate-extra-mode 'c-mode)
       (yas-activate-extra-mode 'yet-another-c-mode)
       (yas-activate-extra-mode 'and-that-one)
       (let* ((expected-first `(and-that-one
                                yet-another-c-mode
                                c-mode
                                ,major-mode))
              (expected-rest `(cc-mode
                               ;; prog-mode doesn't exist in emacs 23.4
                               ,@(if (fboundp 'prog-mode)
                                     '(prog-mode))
                               emacs-lisp-mode
                               and-also-this-one
                               lisp-interaction-mode))
              (observed (yas--modes-to-activate)))
         (should (equal expected-first
                        (cl-subseq observed 0 (length expected-first))))
         (should (equal (sort expected-rest #'string<)
                        (sort (cl-subseq observed (length expected-first)) #'string<))))))))

(defalias 'yas--phony-c-mode 'c-mode)

(ert-deftest issue-492-and-494 ()
  (define-derived-mode yas--test-mode yas--phony-c-mode "Just a test mode")
  (yas-with-snippet-dirs '((".emacs.d/snippets"
                            ("yas--test-mode")))
                         (yas-reload-all)
                         (with-temp-buffer
                           (let* ((major-mode 'yas--test-mode)
                                  (expected `(fundamental-mode
                                              c-mode
                                              ,@(if (fboundp 'prog-mode)
                                                    '(prog-mode))
                                              yas--phony-c-mode
                                              yas--test-mode))
                                  (observed (yas--modes-to-activate)))
                             (should (null (cl-set-exclusive-or expected observed)))
                             (should (= (length expected)
                                        (length observed)))))))

(define-derived-mode yas--test-mode c-mode "Just a test mode")
(define-derived-mode yas--another-test-mode c-mode "Another test mode")

(ert-deftest issue-504-tricky-jit ()
  (yas-with-snippet-dirs
   '((".emacs.d/snippets"
      ("yas--another-test-mode"
       (".yas-parents" . "yas--test-mode"))
      ("yas--test-mode")))
   (let ((b (with-current-buffer (generate-new-buffer "*yas-test*")
              (yas--another-test-mode)
              (current-buffer))))
     (unwind-protect
         (progn
           (yas-reload-all)
           (should (= 0 (hash-table-count yas--scheduled-jit-loads))))
       (kill-buffer b)))))

(defun yas--basic-jit-loading-1 ()
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


;;; Unloading
(ert-deftest yas-unload ()
  "Test unloading and reloading."
  (with-temp-buffer
    (let ((status (call-process
                   (concat invocation-directory invocation-name)
                   nil '(t t) nil
                   "-Q" "--batch" "-L" yas--loaddir "-l" "yasnippet"
                   "--eval"
                   (prin1-to-string
                    '(condition-case err
                         (progn
                           (yas-minor-mode +1)
                           (unload-feature 'yasnippet)
                           ;; Unloading leaves `yas-minor-mode' bound,
                           ;; harmless, though perhaps surprising.
                           (when (bound-and-true-p yas-minor-mode)
                             (error "`yas-minor-mode' still enabled"))
                           (when (fboundp 'yas-minor-mode)
                             (error "`yas-minor-mode' still fboundp"))
                           (require 'yasnippet)
                           (unless (fboundp 'yas-minor-mode)
                             (error "Failed to reload")))
                       (error (message "%S" (error-message-string err))
                              (kill-emacs 1)))))))
      (ert-info ((buffer-string)) (should (eq status 0))))))


;;; Menu
;;;
(defmacro yas-with-even-more-interesting-snippet-dirs (&rest body)
  (declare (debug t))
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
     (let ((menu-items (yas--collect-menu-items
                        (gethash 'fancy-mode yas--menu-table))))
       (should (eql 4 (length menu-items)))
       (dolist (item '("a-guy" "a-beggar"))
         (should (cl-find item menu-items :key #'cl-second :test #'string=)))
       (should-not (cl-find "an-outcast" menu-items :key #'cl-second :test #'string=))
       (dolist (submenu '("sirs" "ladies"))
         (should (keymapp
                  (cl-third
                   (cl-find submenu menu-items :key #'cl-second :test #'string=)))))))))

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
         (should (cl-find item menu :key #'cl-third :test #'string=)))
       (dolist (submenu '("foo-group-a" "foo-group-b"))
         (should (keymapp
                  (cl-fourth
                   (cl-find submenu menu :key #'cl-third :test #'string=))))))
     ;; now group directives
     ;;
     (let ((menu (cdr (gethash 'lisp-interaction-mode yas--menu-table))))
       (should (eql 1 (length menu)))
       (should (cl-find "barbar" menu :key #'cl-third :test #'string=))
       (should (keymapp
                (cl-fourth
                 (cl-find "barbar" menu :key #'cl-third :test #'string=))))))))

(ert-deftest test-group-menus-twisted ()
  "Same as similarly named test, but be mean.

TODO: be meaner"
  (let ((yas-use-menu t))
    (yas-with-even-more-interesting-snippet-dirs
     ;; add a group directive conflicting with the subdir and watch
     ;; behaviour
     (with-temp-buffer
       (insert "# group: foo-group-c\n# --\nstrecmp($1)")
       (write-region nil nil (concat (car (yas-snippet-dirs))
                                     "/c-mode/foo-group-b/strcmp")))
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'c-mode yas--menu-table))))
       (should (eql 4 (length menu)))
       (dolist (item '("printf" "foo-group-a" "foo-group-b" "foo-group-c"))
         (should (cl-find item menu :key #'cl-third :test #'string=)))
       (dolist (submenu '("foo-group-a" "foo-group-b" "foo-group-c"))
         (should (keymapp
                  (cl-fourth
                   (cl-find submenu menu :key #'cl-third :test #'string=))))))
     ;; delete the .yas-make-groups file and watch behaviour
     ;;
     (delete-file (concat (car (yas-snippet-dirs))
                          "/c-mode/.yas-make-groups"))
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'c-mode yas--menu-table))))
       (should (eql 5 (length menu))))
     ;; Change a group directive and reload
     ;;
     (let ((menu (cdr (gethash 'lisp-interaction-mode yas--menu-table))))
       (should (cl-find "barbar" menu :key #'cl-third :test #'string=)))

     (with-temp-buffer
       (insert "# group: foofoo\n# --\n(ert-deftest ${1:name} () $0)")
       (write-region nil nil (concat (car (yas-snippet-dirs))
                                     "/lisp-interaction-mode/ert-deftest")))
     (yas-reload-all 'no-jit)
     (let ((menu (cdr (gethash 'lisp-interaction-mode yas--menu-table))))
       (should (eql 1 (length menu)))
       (should (cl-find "foofoo" menu :key #'cl-third :test #'string=))
       (should (keymapp
                (cl-fourth
                 (cl-find "foofoo" menu :key #'cl-third :test #'string=))))))))


;;; The infamous and problematic tab keybinding
;;;
(ert-deftest test-yas-tab-binding ()
  (yas-saving-variables
   (yas-with-snippet-dirs
    '((".emacs.d/snippets"
       ("fundamental-mode"
        ("foo" . "foobar"))))
    (yas-reload-all)
    (with-temp-buffer
      (yas-minor-mode -1)
      (insert "foo")
      (should (not (eq (key-binding (yas--read-keybinding "<tab>")) 'yas-expand)))
      (yas-minor-mode 1)
      (should (eq (key-binding (yas--read-keybinding "<tab>")) 'yas-expand))
      (yas-expand-snippet "$1 $2 $3")
      (should (eq (key-binding [(tab)]) 'yas-next-field-or-maybe-expand))
      (should (eq (key-binding (kbd "TAB")) 'yas-next-field-or-maybe-expand))
      (should (eq (key-binding [(shift tab)]) 'yas-prev-field))
      (should (eq (key-binding [backtab]) 'yas-prev-field))))))

(ert-deftest test-rebindings ()
  (let* ((yas-minor-mode-map (copy-keymap yas-minor-mode-map))
         (minor-mode-map-alist
          (cons `(yas-minor-mode . ,yas-minor-mode-map)
                (cl-remove 'yas-minor-mode minor-mode-map-alist
                           :test #'eq :key #'car))))
    (define-key yas-minor-mode-map [tab] nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)
    (with-temp-buffer
      (yas-minor-mode 1)
      (should-not (eq (key-binding (kbd "TAB")) 'yas-expand))
      (should (eq (key-binding (kbd "SPC")) 'yas-expand))
      (yas-reload-all)
      (should-not (eq (key-binding (kbd "TAB")) 'yas-expand))
      (should (eq (key-binding (kbd "SPC")) 'yas-expand)))))

(ert-deftest test-yas-in-org ()
  (yas-saving-variables
   (yas-with-snippet-dirs
    '((".emacs.d/snippets"
       ("org-mode"
        ("foo" . "foobar"))))
    (yas-reload-all)
    (with-temp-buffer
      (org-mode)
      (yas-minor-mode 1)
      (insert "foo")
      (should (eq (key-binding [(tab)]) 'yas-expand))
      (should (eq (key-binding (kbd "TAB")) 'yas-expand))))))

(ert-deftest yas-org-native-tab-in-source-block-text ()
  "Test expansion of snippets in org source blocks."
  ;; org 9+ no longer runs fontification for text-mode, so our hacks
  ;; don't work.  Note that old ert doesn't have skipping, so we have
  ;; to expect failure instead.
  :expected-result (if (and (fboundp 'org-in-src-block-p)
                            (version< (org-version) "9"))
                       :passed :failed)
  (let ((text-mode-hook #'yas-minor-mode))
    (do-yas-org-native-tab-in-source-block "text")))

(ert-deftest yas-org-native-tab-in-source-block-emacs-lisp ()
  "Test expansion of snippets in org source blocks."
  :expected-result (if (fboundp 'org-in-src-block-p)
                       :passed :failed)
  (let ((emacs-lisp-mode-hook #'yas-minor-mode)
        ;; This makes the test a bit less comprehensive, but it's
        ;; needed to avoid bumping into Emacs Bug#35264.
        (org-src-preserve-indentation t))
    (do-yas-org-native-tab-in-source-block "emacs-lisp")))

(defun do-yas-org-native-tab-in-source-block (mode)
  (yas-saving-variables
   (yas-with-snippet-dirs
    `((".emacs.d/snippets"
       (,(concat mode "-mode")
        ("T" . "${1:one} $1\n${2:two} $2\n<<$0>> done!"))))
    ;; Binding both text and prog mode hook should cover everything.
    (let ((org-src-tab-acts-natively t)
          ;; Org 8.x requires this in order for
          ;; `org-src-tab-acts-natively' to have effect.
          (org-src-fontify-natively t))
      (yas-reload-all)
      ;; Org relies on font-lock to identify source blocks.
      (yas--with-font-locked-temp-buffer
       (org-mode)
       (yas-minor-mode 1)
       (insert "#+BEGIN_SRC " mode "\nT\n#+END_SRC")
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (jit-lock-fontify-now))
       (re-search-backward "^T$") (goto-char (match-end 0))
       (should (org-in-src-block-p))
       (ert-simulate-command `(,(key-binding (kbd "TAB"))))
       (ert-simulate-command `(,(key-binding (kbd "TAB"))))
       (ert-simulate-command `(,(key-binding (kbd "TAB"))))
       ;; Check snippet exit location.
       (should (looking-at ">> done!"))
       (goto-char (point-min))
       (forward-line)
       ;; Check snippet expansion, ignore leading whitespace due to
       ;; `org-edit-src-content-indentation'.
       (should (looking-at "\
\[[:space:]]*one one
\[[:space:]]*two two
\[[:space:]]*<<>> done!")))))))


(ert-deftest test-yas-activate-extra-modes ()
  "Given a symbol, `yas-activate-extra-mode' should be able to
add the snippets associated with the given mode."
  (with-temp-buffer
    (yas-saving-variables
     (yas-with-snippet-dirs
       '((".emacs.d/snippets"
          ("markdown-mode"
           ("_" . "_Text_ "))
          ("emacs-lisp-mode"
           ("car" . "(car )"))))
       (yas-reload-all)
       (emacs-lisp-mode)
       (yas-minor-mode +1)
       (yas-activate-extra-mode 'markdown-mode)
       (should (eq 'markdown-mode (car yas--extra-modes)))
       (yas-should-expand '(("_" . "_Text_ ")))
       (yas-should-expand '(("car" . "(car )")))
       (yas-deactivate-extra-mode 'markdown-mode)
       (should-not (eq 'markdown-mode (car yas--extra-modes)))
       (yas-should-not-expand '("_"))
       (yas-should-expand '(("car" . "(car )")))))))



(provide 'yasnippet-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; autoload-compute-prefixes: nil
;; End:
;;; yasnippet-tests.el ends here
