;;; snippet-tests.el --- some basic tests for snippet.el  -*- lexical-binding: t; -*-

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

(define-snippet printf ()
  "printf (\""
  (field 1 "%s")
  (mirror 1 (if (string-match "%" field-text) "\"," "\);"))
  (field 2)
  (mirror 1 (if (string-match "%" field-text) "\);" "")))

(define-snippet foo ()
  (field 1 "bla")
  "ble"
  (mirror 1)
  (field 2
         ((field 3 "fonix")
          "fotrix"
          (mirror 1 (concat field-text "qqcoisa"))))
  "end")

(define-snippet easy ()
  "A "
  (field 1 "field")
  " and its mirror: "
  (mirror 1 (format "(mirror of %s)" field-text)))

(defun test ()
  (interactive)
  (with-current-buffer (switch-to-buffer (get-buffer-create "*test easy snippet*"))
    (erase-buffer)
    (easy)))

(defun test2 ()
  (interactive)
  (with-current-buffer (switch-to-buffer (get-buffer-create "*test printf snippent*"))
    (erase-buffer)
    (printf)))

(provide 'snippet-tests)

;;; Local Variables:
;;; lexical-binding: t
;;; End:
;;; snippet-tests.el ends here
