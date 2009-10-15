(defvar yas/ruby-snippet-open-paren " "
  "The open parenthesis used in ruby-mode snippets. Normally blank but could be (")
(defvar yas/ruby-snippet-close-paren " "
  "The close parenthesis used in ruby-mode snippets. Normally blank but could be )")
(defun yas/ruby-snippet-paren (&optional arg)
  "Defaults to returning the open paren. If arg equals t then shows close paren."
  (if arg
      yas/ruby-snippet-close-paren
    yas/ruby-snippet-open-paren))

(defun yas/ruby-infer-class-name ()
  "Infer the class name from the buffer. Thanks to hitesh <hitesh.jasani@gmail.com>"
  (let ((fn (capitalize (file-name-nondirectory
                         (file-name-sans-extension
                          (buffer-file-name))))))
    (cond
     ((string-match "_" fn) (replace-match "" nil nil fn))
     (t fn))))

(defun yas/ruby-in-interpolated-string-p ()
  (eq (fourth (syntax-ppss))
      ?\"))

(defun yas/ruby-in-comment-p ()
  (fifth (syntax-ppss)))


