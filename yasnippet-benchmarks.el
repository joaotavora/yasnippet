(require 'yasnippet-tests)

(defvar manywords (with-temp-buffer (insert-file-contents "/usr/share/dict/words")
                                    (split-string (buffer-string))))


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
      (erase-buffer)
      ;; jit, no compiled, fastest
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         ;; (yas/recompile-all)
                         (let ((start (current-time)))
                           (yas/reload-all)
                           (insert (format "JIT    NO-COMPILED: %s s\n" (float-time (time-subtract (current-time) start))))))
      ;; jit, compiled, this is equivalent to the first one. The real work
      ;; will be done when modes are effectively accessed
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (let ((start (current-time)))
                           (yas/reload-all)
                           (insert (format "JIT       COMPILED: %s s\n" (float-time (time-subtract (current-time) start))))))
      ;; no-jit no-compiled files, slow
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (let ((start (current-time))
                               (yas/auto-compile-snippets nil))
                           (yas/reload-all t)
                           (insert (format "NO-JIT NO-COMPILED: %s s\n" (float-time (time-subtract (current-time) start))))))
      ;; no-jit compiled files, this is what normally happens when a user
      ;; interactive calls `yas/reload-all'
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (yas/recompile-all)
                         (let ((start (current-time))
                               (yas/auto-compile-snippets nil))
                           (yas/reload-all t)
                           (insert (format "NO-JIT    COMPILED: %s s (no recompilation-needed, no auto-compilation checks)\n" (float-time (time-subtract (current-time) start))))))
      ;; no-jit compiled files, this is what normally happens when a
      ;; user interactive calls `yas/reload-all', but now with
      ;; auto-compilation `mtime' checks
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                             (yas/recompile-all)
                             (let ((start (current-time))
                                   (yas/auto-compile-snippets t))
                               (yas/reload-all t)
                               (insert (format "NO-JIT    COMPILED: %s s (no recompilation needed, auto-compilation checks)\n" (float-time (time-subtract (current-time) start))))))
      ;; no-jit no previous compilation, the autocompilation case, slowest
      ;;
      (yas/with-snippet-dirs (list dummy-snippets)
                         (let ((start (current-time))
                               (yas/auto-compile-snippets t))
                           (yas/reload-all t)
                           (insert (format "NO-JIT NO-COMPILED: %s s (recompilation needed, auto-compilation kicks in all dirs)\n" (float-time (time-subtract (current-time) start))))))
      (pop-to-buffer (current-buffer)))))
