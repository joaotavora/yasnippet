(require 'yasnippet-tests)

(defvar manywords (with-temp-buffer (insert-file-contents "/usr/share/dict/words")
                                    (split-string (buffer-string))))


(defmacro yas/time (&rest body)
  (let ((start-sym (gensym)))
    `(let ((,start-sym (current-time)))
       ,@body
       (float-time (time-subtract (current-time) ,start-sym)))))

(defun yas/run-some-benchmarks ()
  (interactive)
  (let* ((dummy-mode-names (subseq manywords 0 10))
         (dummy-snippet-names (subseq manywords 200 300))
         (dummy-snippets `("testsnippets"
                           ,@(mapcar #'(lambda (dummy-mode-name)
                                         `(,dummy-mode-name
                                           ,@(mapcar #'(lambda (dummy-snippet-name)
                                                         `(,dummy-snippet-name . ,(concat dummy-snippet-name "'s content")))
                                                     dummy-snippet-names)))
                                     dummy-mode-names)))
         (yas/verbosity 1))
    (with-current-buffer (get-buffer-create "*yas/benchmarks*")
      (end-of-buffer)
      (insert (format "\nComparison of `yas/reload-all' performance: (%s)\n\n" (current-time-string)))
      ;; jit, no compiled, fastest
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         ;; (yas/recompile-all)
                         (let ((yas/auto-compile-snippets nil))
                           (insert (format "JIT    NO-COMPILED NO-AUTOCOMP: %s s\n"
                                           (yas/time (yas/reload-all))))))
      ;; jit, compiled, this is equivalent to the first one. The real work
      ;; will be done when modes are effectively accessed
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (let ((yas/auto-compile-snippets nil))
                           (insert (format "JIT       COMPILED NO-AUTOCOMP: %s s (identical to the previous case, no snippets are actually loaded)\n"
                                           (yas/time (yas/reload-all))))))
      ;; no-jit no-compiled files, slow
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (let ((yas/auto-compile-snippets nil))
                           (insert (format "NO-JIT NO-COMPILED NO-AUTOCOMP: %s s (every snippet loaded from file)\n"
                                           (yas/time (yas/reload-all t))))))
      ;; no-jit compiled files, this is what normally happens when a user
      ;; interactive calls `yas/reload-all'
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (yas/recompile-all)
                         (let ((yas/auto-compile-snippets nil))
                           (insert (format "NO-JIT    COMPILED NO-AUTOCOMP: %s s (no recompilation-needed)\n"
                                           (yas/time (yas/reload-all t))))))
      ;; no-jit compiled files, this is what normally happens when a
      ;; user interactive calls `yas/reload-all', but now with
      ;; auto-compilation `mtime' checks
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                             (yas/recompile-all)
                             (let ((yas/auto-compile-snippets t))
                               (insert (format "NO-JIT    COMPILED    AUTOCOMP: %s s (no recompilation needed, just compilation checks)\n"
                                               (yas/time (yas/reload-all t))))))
      ;; no-jit no previous compilation, the autocompilation case, slowest
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (let ((yas/auto-compile-snippets t))
                           (insert (format "NO-JIT NO-COMPILED    AUTOCOMP: %s s (recompilation needed, auto-compilation kicks in all dirs)\n"
                                           (yas/time (yas/reload-all t))))))
      (pop-to-buffer (current-buffer)))))
