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

;;; Helpers
;;; 

(defun yas/mock-insert (string)
  (interactive)
  (do ((i 0 (1+ i)))
      ((= i (length string)))
    (insert (aref string i))))


(provide 'yasnippet-tests)
;;; yasnippet-tests.el ends here
