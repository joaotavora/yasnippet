(defvar yas/ruby-snippet-open-paren " "
  "The open parenthesis used in ruby-mode snippets. Normally blank but could be (")
(defvar yas/ruby-snippet-close-paren " "
  "The close parenthesis used in ruby-mode snippets. Normally blank but could be )")
(defun yas/ruby-snippet-paren (&optional arg)
  "Defaults to returning the open paren. If arg equals t then shows close paren."
  (if arg
      yas/ruby-snippet-close-paren
    yas/ruby-snippet-open-paren))

(defvar yas/ruby-shebang-args " -wKU"
  "Arguments for the ruby shebang line.")

(defun yas/ruby-infer-class-name ()
  "Infer the class name from the buffer. Thanks to hitesh <hitesh.jasani@gmail.com>"
  (let ((fn (capitalize (file-name-nondirectory
                         (file-name-sans-extension
                          (buffer-file-name))))))
    (cond
     ((string-match "_" fn) (replace-match "" nil nil fn))
     (t fn))))

(defun yas/ruby-toggle-single-multi-line-block ()
  (interactive)
  (save-excursion
    (let* ((block-start (progn (ruby-beginning-of-block) (point)))
           (block-end (progn (ruby-end-of-block) (forward-word)(point))))
      (when (and block-start
                 block-end)
        (goto-char block-start)
        (cond ((not (eq (line-number-at-pos block-end)
                        (line-number-at-pos block-start)))
               (when (looking-at ".*[^\w]\\(do\\)[^\w]\\(|.*|\\)?")
                 (goto-char block-end)
                 (insert "}")
                 (goto-char block-start)
                 (replace-regexp "do" "{" t block-start (line-end-position))))
              (t
               (replace-regexp "\\(.*\\){[^w]\\(|.*|\\)" "\1do \2\n" nil block-start block-end)))))))


;; conditions
;; 
(yas/define-condition-cache yas/ruby-in-interpolated-string-p (member (fourth (syntax-ppss)) (list ?\" ?\`)))
(yas/define-condition-cache yas/ruby-in-comment-p (fifth (syntax-ppss)))
(yas/define-condition-cache yas/ruby-in-string-p (fourth (syntax-ppss)))
(yas/define-condition-cache yas/ruby-end-is-block-end-p
                            (save-excursion
                              (ruby-backward-sexp)
                              (not (eq (point) (point-min)))))