(require 'tools)

(defvar *default-compilation-file* "Makefile")

(comment
 "I use it like this"
 (global-set-key [(f9)] 'smart-compile)

 (eval-after-load "compile"
   '(setq compilation-process-setup-function 'reach-compilation-file))

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
             (let ((must-start-swank (not (get-buffer "*compilation*")))
                   (must-connect-to-slime (not (slime-connected-p))))
               (cond (must-start-swank (compile compile-command))
                     (must-connect-to-slime (slime-connect "127.0.0.1" 4005))
                     (t
                      (unless (symbol-value 'clojure-test-mode)
                        (-?>> (buffer-file-name)
                              (replace-regexp-in-string "\\.clj$" "_test.clj")
                              (replace-regexp-in-string "/src/" "/test/")
                              find-file))
                      (clojure-test-run-tests)))))
           (make-or-rake ()
             (when (and (string-match  "make" compile-command)
                        (null (nearest-compilation-file default-directory
                                                        *default-compilation-file*)))
               (message "No Makefile found, switching to rake")
               (set (make-local-variable 'compile-command) "rake"))
             (compile compile-command)))
    (cond (eldoc-mode (run-elisp-tests))
          ((eq major-mode 'clojure-mode) (run-clojure-tests))
          (t (make-or-rake)))))

(defun nearest-compilation-file (dir compilation-file)
  "Search for the compilation file traversing up the directory tree."
  (let ((file-path (concat dir compilation-file))
        (parent (file-name-directory (directory-file-name dir))))
    (cond
      ((file-exists-p file-path) file-path)
      ((string= dir parent) nil)
      (t (nearest-compilation-file parent compilation-file)))))

(defun reach-compilation-file ()
  "If your compile command containts 'make', goes up in the path
until it finds a makefile."
  (when (string-match "make"
                      (car compilation-arguments))
    (let ((compilation-file (nearest-compilation-file (expand-file-name default-directory)
                                                      *default-compilation-file*)))
      (when (null compilation-file)
        (error "No file named '%s' found" *default-compilation-file*))
      (setq default-directory (file-name-directory compilation-file)))))

(defun compile-goto-error-and-close-compilation-window ()
  "Useful to close compilation windows, so you have only one
window open at any time (I set `pop-up-windows' to nil)."
  (interactive)
  (compile-goto-error)
  (delete-windows-on "*compilation*"))

(provide 'smart-compile)
