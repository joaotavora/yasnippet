(defvar yas/rails-root-cache nil)

;; stolen from rinari-mode's rinari-root
(defun yas/rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (or yas/rails-root-cache
      (if (file-exists-p (expand-file-name
                          "environment.rb" (expand-file-name "config" dir)))
          (set (make-local-variable 'yas/rails-root-cache) dir)
        (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
          ;; regexp to match windows roots, tramp roots, or regular posix roots
          (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
            (yas/rails-root new-dir))))))

;; stolen from rinari-mode's rinari-extract-partial
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
;;;
;;; The TextMate "intelligent" migration snippet
;;
(defvar yas/rails-intelligent-migration-snippet-bits
      '((:rename_column . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}$0")
                           (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_column_continue . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}\nmncc$0")
                                    (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_table . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}$0")
                          (:down . "rename_table :$2, :$1" )))

        (:rename_table_continue . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}\nmntc$0")
                                   (:down . "rename_table :$2, :$1" )))

        (:add_remove_column . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}$0")
                               (:down . "remove_column :$1, :$2" )))
        
        (:add_remove_column_continue . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}\nmarcc$0")
                                        (:down . "remove_column :$1, :$2" )))
        
        (:remove_add_column . ((:up   . "remove_column :${1:table_name}, :${2:column_name}$0")
                               (:down . "add_column :$1, :$2, :$3{string}" )))

        (:create_drop_table . ((:up   . "create_table :${1:table_name}, :force . true do |t|\nt.$0\nt.timestamps\nend")
                               (:down . "drop_table :$1" )))

        (:change_change_table . ((:up   . "change_table :${1:table_name} do |t|\nt.$0\nend")
                                 (:down . "change_table :$1 do |t|\nend" )))

        (:add_remove_index . ((:up   . "add_index :${1:table_name}, :${2:column_name}$0")
                              (:down . "remove_index :$1, :$2" )))

        (:add_remove_unique_index . ((:up   . "add_index :${1:table_name}, ${2:[:${3:column_name}${4:, :${5:column_name}}]}, :unique . true$0")
                                     (:down . "remove_index :$1, :column . $2" )))

        (:add_remove_named_index . ((:up   . "add_index :${1:table_name}, [:${2:column_name}${3:, :${4:column_name}}], :name . \"${5:index_name}\"${6:, :unique . true}$0")
                                    (:down . "remove_index :$1, :name . :$5" )))))


(defun yas/rails-intelligent-migration-snippet (type)
  (let* ((start  (point))
         (end (save-excursion
                (search-forward-regexp "^\s*def\sself\.down" nil 'noerror)))
         (up (aget (aget yas/rails-intelligent-migration-snippet-bits type) :up))
         (down (aget (aget yas/rails-intelligent-migration-snippet-bits type) :down))
         (snippet
          (and up down start end (concat up
                                         (buffer-substring-no-properties start end)
                                         "\n" down))))
    (when snippet
      (delete-region start end)
      (yas/expand-snippet snippet))))

(yas/define-condition-cache
  yas/rails-intelligent-migration-snippet-condition-p
  "Non-nil if an \"intelligent\" migration snippet should be expanded"
  (and (yas/rails-migration-p)
       (not (yas/rails-in-create-table-p))
       (not (yas/rails-in-change-table-p))
       (yas/rails-in-ruby-block-like "self\.up")))

(defun yas/rails-in-ruby-block-like (regexp)
  (save-excursion
    (ruby-accurate-end-of-block)
    (ruby-backward-sexp)
    (search-forward-regexp regexp (line-end-position) t)))

;;; conditions
(yas/define-condition-cache
 yas/rails-in-create-table-p
 "Non-nil if point is inside a 'create_table' method call."
 (yas/rails-in-ruby-block-like "create_table"))

(yas/define-condition-cache
 yas/rails-in-change-table-p
 "Non-nil if point is inside a 'change_table' method call."
 (yas/rails-in-ruby-block-like "change_table"))

(yas/define-condition-cache
 yas/rails-model-p
 "Non-nil if the current buffer is a rails model."
 (and (yas/rails-root)
      (string-match "app/models/$" default-directory)))

(yas/define-condition-cache
 yas/rails-view-p
 "Non-nil if the current buffer is a rails view."
 (and (yas/rails-root)
      (string-match "app/views/" default-directory)))

(yas/define-condition-cache
 yas/rails-controller-p
"Non-nil if the current buffer is a rails controller." 
 (and (yas/rails-root)
      (string-match "app/controllers/$" default-directory)))

(yas/define-condition-cache
 yas/rails-migration-p
 "Non-nil if the current buffer is a rails migration."
 (and (yas/rails-root)
      (string-match "db/migrate/" default-directory)))

(defadvice cd (after yas/rails-on-cd-activate activate)
  "Set `yas/mode-symbol' to `rails-mode' so that rails snippets
are recognized"
  (setq yas/rails-root-cache nil)
  (when (yas/rails-root)
    (set (make-local-variable 'yas/mode-symbol) 'rails-mode))) 
