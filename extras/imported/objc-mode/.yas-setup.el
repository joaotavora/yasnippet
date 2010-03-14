;;; constants
(defvar yas/objc-void-regexp "\\(void\\|IBAction\\)")


;;; helper stuff
(defun yas/objc-guess-instance-name (text)
  (if (string-match "NS\\(\\([AEIOQUY]\\)?[^ *]*\\)" text)
      (if (match-beginning 2)
          (concat "an" (capitalize (match-string-no-properties 1 text)))
        (concat "a" (capitalize (match-string-no-properties 1 text))))
    "arg"))

(defun yas/objc-guess-member-name (text)
  "Turns FOOBARBaz to baz"
  (let ((case-fold-search nil))
    (if (string-match "[A-Z]+\\([A-Z]\\)\\(.*\\)" text)
        (concat (downcase (match-string-no-properties 1 text))
                (match-string-no-properties 2 text))
      text)))

;;; cached conditions
(yas/define-condition-cache
 yas/objc-interface-p
 "Non-nil if point inside an objc @interface declaration."
 (yas/objc-in-c-block-like "@interface"))

(yas/define-condition-cache
 yas/objc-implementation-p
 "Non-nil if point inside an objc @implementation declaration."
 (yas/objc-in-c-block-like "@implementation"))

(yas/define-condition-cache
 yas/objc-method-body-p
 "Non-nil if point inside an objc method definition."
 (yas/objc-in-c-block-like 'objc-method-intro))

;;; helpers for cached conditions 
(defun yas/objc-in-c-block-like (symbol-or-regexp)
  (let ((original-point (point))
        (start-point nil))
    (save-excursion
      (cond ((symbolp symbol-or-regexp)
             (c-beginning-of-defun)
             (setq start-point (point))
             (let ((syntax-info (c-guess-basic-syntax)))
               (when (and syntax-info
                          (eq (caar syntax-info) symbol-or-regexp))
                 (c-end-of-defun)
                 (and (< start-point original-point)
                      (< original-point (point))))))
            ((stringp symbol-or-regexp)
             (when (search-backward-regexp symbol-or-regexp
                                           nil
                                           t)
               (setq start-point (point))
               (when (search-forward-regexp "^@end" nil t)
                 (and (< start-point original-point)
                      (< original-point (point))))))))))
