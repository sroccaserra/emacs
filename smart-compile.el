(require 'tools)

(defvar *default-compilation-file* "Makefile")

(comment
 "I use it like this"
 (global-set-key [(f9)] 'smart-compile)

 (eval-after-load "compile"
   '(define-key
     compilation-mode-map
     [remap compile-goto-error]
     'compile-goto-error-and-close-compilation-window)))

(defun smart-compile ()
  "Saves current buffer, and depending on context:

- in an Elisp file, runs elk tests.

- in a Clojure file, runs clojure-test-run-tests.

- if the compile command contains \"make\", and a makefile is
  found in the current directory or upward, runs the compile
  command.

- otherwise, switches to rake for the current buffer and
  compiles."
  (interactive)
  (save-buffer)
  (labels ((run-elisp-tests ()
             (eval-buffer)
             (when (fboundp 'elk-test-run-all-buffers)
               (elk-test-run-all-buffers t)))
           (run-clojure-tests ()
             (cond ((slime-connected-p)
                    (unless (symbol-value 'clojure-test-mode)
                      (-?>> (buffer-file-name)
                            (replace-regexp-in-string "\\.clj$" "_test.clj")
                            (replace-regexp-in-string "/src/" "/test/")
                            find-file))
                    (clojure-test-run-tests))
                   (t (when (y-or-n-p "Slime is not connected, open a terminal?")
                        (shell-command "cygterm")))))
           (make-or-rake ()
             (let ((compilation-file (nearest-compilation-file default-directory)))
               (when (and (not (string-match  "rake" compile-command))
                          (null compilation-file))
                 (message "No Makefile found, switching to rake")
                 (set (make-local-variable 'compile-command) "rake"))
               (let ((default-directory (if compilation-file
                                            (file-name-directory compilation-file)
                                          default-directory)))
                 (compile compile-command)))))
    (cond (eldoc-mode (run-elisp-tests))
          ((eq major-mode 'clojure-mode) (run-clojure-tests))
          (t (make-or-rake)))))

(defun nearest-compilation-file (dir)
  "Search for the compilation file traversing up the directory tree."
  (let ((file-path (concat dir *default-compilation-file*))
        (parent (file-name-directory (directory-file-name dir))))
    (cond
      ((file-exists-p file-path) file-path)
      ((string= dir parent) nil)
      (t (nearest-compilation-file parent)))))

(defun compile-goto-error-and-close-compilation-window ()
  "Useful to close compilation windows, so you have only one
window open at any time (I set `pop-up-windows' to nil)."
  (interactive)
  (compile-goto-error)
  (delete-windows-on "*compilation*"))

(provide 'smart-compile)
