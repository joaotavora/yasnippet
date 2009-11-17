(defvar yas/rails-root-cache nil)
(make-local-variable 'yas/rails-root-cache)

;; copied from rinari-mode's rinari-root
(defun yas/rails-root (&optional dir home)
  (or yas/rails-root-cache
      (or dir (setq dir default-directory))
      (if (file-exists-p (expand-file-name
                          "environment.rb" (expand-file-name "config" dir)))
          (setq yas/rails-root-cache dir)
        (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
          ;; regexp to match windows roots, tramp roots, or regular posix roots
          (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
            (rinari-root new-dir))))))

;; copied from rinari-mode's rinari-extract-partial
(defun yas/rails-extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (let* ((path (buffer-file-name)) ending)
    (if (string-match "view" path)
	(let ((ending (and (string-match ".+?\\(\\.[^/]*\\)$" path)
			   (match-string 1 path)))
	      (partial-name
	       (replace-regexp-in-string "[[:space:]]+" "_" partial-name)))
	  (kill-region begin end)
	  (if (string-match "\\(.+\\)/\\(.+\\)" partial-name)
	      (let ((default-directory (expand-file-name (match-string 1 partial-name)
							 (expand-file-name ".."))))
		(find-file (concat "_" (match-string 2 partial-name) ending)))
	    (find-file (concat "_" partial-name ending)))
	  (yank) (pop-to-buffer nil)
	  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))
      (message "not in a view"))))

(defun yas/rails-model-p ()
  (and (yas/rails-root)
       (string-match "app/models/$" default-directory)))

(defun yas/rails-view-p ()
  (and (yas/rails-root)
       (string-match "app/views/" default-directory)))

(defun yas/rails-controller-p ()
  (and (yas/rails-root)
       (string-match "app/controllers/$" default-directory)))

(defun yas/rails-migration-p ()
  (and (yas/rails-root)
       (string-match "db/migrate/" default-directory)))


