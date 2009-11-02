(defvar yas/html-default-tag "p")

(defvar yas/html-xhtml-attr "")

(defvar yas/html-just-like-tm nil
  "Html-mode snippets behave as close to TextMate as possible.")


(defun yas/html-remove-preceding-word ()
  (interactive)
  (let (word-begin
        word-end
        (orig-point (point))
        retval)
    (save-excursion
      (forward-word -1)
      (setq word-begin (point))
      (forward-word 1)
      (setq word-end (point)))
    (when (and (> word-end word-begin)
               (>= word-end (point)))
      (setq retval
            (cons
             (buffer-substring-no-properties word-begin orig-point)
             (buffer-substring-no-properties word-end orig-point)))
      (delete-region word-begin word-end)
      retval)))


(defun yas/html-first-word (string)
  (replace-regexp-in-string "\\\W.*" "" string))

(defun yas/html-insert-tag-pair-snippet ()
  (let* ((tag-and-suffix (or (and yas/selected-text
                                  (cons yas/selected-text nil))
                             (yas/html-remove-preceding-word)))
         (tag    (car tag-and-suffix))
         (suffix (or (cdr tag-and-suffix) ""))
         (single-no-arg "\\(br\\|hr\\)")
         (single        "\\(img\\|meta\\|link\\|input\\|base\\|area\\|col\\|frame\\|param\\)"))
    (cond ((null tag)
           (yas/expand-snippet (format "<${1:%s}>%s</${1:$(yas/html-first-word yas/text)}>%s"
                                       (or yas/html-default-tag
                                           "p")
                                       (if yas/html-just-like-tm "$2" "$0")
                                       suffix)))
          ((string-match single-no-arg tag)
           (insert (format "<%s%s/>%s" tag yas/html-xhtml-attr suffix)))
          ((string-match single tag)
           (yas/expand-snippet (format "<%s $1%s/>%s" tag yas/html-xhtml-attr suffix)))
          (t
           (yas/expand-snippet (format "<%s>%s</%s>%s"
                                       tag
                                       (if yas/html-just-like-tm "$1" "$0")
                                       (replace-regexp-in-string "\\\W.*" "" tag)
                                       suffix))))))

(defun yas/html-wrap-each-line-in-openclose-tag ()
  (let* ((mirror "${1:$(yas/html-first-word yas/text)}")
         (template (concat (format "<${1:%s}>" (or yas/html-default-tag "p"))
                           yas/selected-text
                           "</" mirror ">")))
    (setq template (replace-regexp-in-string "\n" (concat "</" mirror ">\n<" mirror ">") template))
    (yas/expand-snippet template)))

(defun yas/html-between-tag-pair-p ()
  (save-excursion
    (backward-word)
    (looking-at "\\\w+></\\\w+>")))


