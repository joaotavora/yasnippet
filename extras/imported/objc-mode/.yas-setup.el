;;; conditions
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
 "Non-nil if point inside an objc @implementation declaration."
 (yas/objc-in-c-block-like 'objc-method-intro))


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
