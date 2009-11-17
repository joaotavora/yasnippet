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
    (delete-region start end)
    (yas/expand-snippet snippet)))
