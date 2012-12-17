;; -*-Emacs-Lisp-*-

(defun yas-strip-selected-text ()
  "strip leading whitespace, first and last blank line."
  (replace-regexp-in-string
   ".*\\(\n\\)\\'" ""
   (replace-regexp-in-string
    "\\`\\(\n\\).*" ""
    (replace-regexp-in-string
     "\\`\\([ \t]+\\).*" ""
     yas-selected-text nil nil 1) nil nil 1) nil nil 1))

(defun yas-downcase (string)
  "split capitalize word with underscore, then downcase."
  (downcase (replace-regexp-in-string "[a-z]\\([A-Z]\\)" "_\\&" string nil nil 1)))

(defun yas-capitalize (string)
  "merge underscore-split word into a capitalize form."
  (replace-regexp-in-string "_\\|@\\|\\$" "" (capitalize string)))

(defun erase (string)
  "erase those string when this field content is empty."
  (and (string-match "[^\s\t]" yas-text) string))

(defun erase-if-quote (string)
  "erase those string when this field content contain quote mark."
  (and (not (string-match "^[\"']\\|^[\s\t]*$" yas-text)) string))

(defun add_if_comma (string)
  "add those string when this field content contain a comma."
  (and (string-match "," yas-text) string))

(defun add (string)
  "add those string when this field content is empty."
  (and (not (yas-text)) string))

(defun erase1 (string)
  "erase those string when region is a one-line string."
  (and (string-match "\n" yas-selected-text) string))

(defun add1 (string)
  "add those string when region is a one-line string."
  (and (not (string-match "\n" yas-selected-text)) string))

(defun end ()
  "add a trailing newline when region is multi-line string."
  (and (string-match "\n" yas-selected-text) "
"))

(defun space ()
  "add a space when region is multi-line string."
  (and (string-match "\n" yas-selected-text) " "))
