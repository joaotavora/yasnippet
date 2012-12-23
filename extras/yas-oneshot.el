;;; yas-oneshot.el --- create a snippet fast

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: create a snippet fast
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Mon Dec 24 01:08:15 2012 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 11
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;;     (require 'yas-oneshot)
;;
;;

;;; Commentary:

;; Keybinding is "C-c & o", see doc of `yas-oneshot' for more info.
;;
;; - Quickly create a snippet from the region.
;; - Choose from found symbols as placeholders.
;; - Dump snippet into file, if saving.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'yasnippet)

(defvar yas-oneshot-snippet nil)

;;;###autoload
(defun yas-oneshot (dump &optional start end)
  "with region selected, convert region into oneshot snippet
with universal arg, dump current oneshot snippet
else expand current oneshot snippet"
  (interactive (cond
                (current-prefix-arg
                 (list 'dump nil nil))
                ((use-region-p)
                 (list nil (region-beginning) (region-end)))
                (t
                 (list nil))))
  (cond
   (dump
    (insert yas-oneshot-snippet))
   ((and start end)
    (setq deactivate-mark t)
    (yas-oneshot-register start end))
   (yas-oneshot-snippet
    (yas-expand-snippet yas-oneshot-snippet))
   (t
    (error "not sure what to do."))))

(defun yas-oneshot-register (start end)
  (setq yas-oneshot-snippet (buffer-substring-no-properties start end))
  (loop with symbols =  (cons "" (remove-duplicates
                                (save-excursion
                                  (goto-char start)
                                  (loop
                                   while (re-search-forward "\\_<.*?\\_>" end t)
                                   for sym = (match-string-no-properties 0)
                                   collect sym))
                                :test 'equal))
        with res
        for index from 1
        do (progn
             (setq res (completing-read (format "field %s: " index) symbols))
             (unless (string= "" res)
               (let ((start-index 0)
                     temp)
                 (while (string-match res yas-oneshot-snippet start-index)
                   (if (= start-index 0)
                       (setq temp (format "${%s:%s}" index res))
                     (setq temp (format "$%s" index)))
                   (setq yas-oneshot-snippet (replace-match temp nil 'literal yas-oneshot-snippet))
                   (setq start-index (+ (match-beginning 0) (length temp)))))))
        while (not (string= "" res))))

(defun yas-oneshot-define-key ()
  "add keybinding to minor-mode"
  (define-key yas-minor-mode-map [(control c) (&) (o)] 'yas-oneshot))

(yas-oneshot-define-key)

;; we can't use `eval-after-load' here because `yas-reload-all' resets the keymap.

;;;###autoload
(defadvice yas-reload-all (after load-oneshot activate)
  (yas-oneshot-define-key))



(provide 'yas-oneshot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yas-oneshot.el ends here

