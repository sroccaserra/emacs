(defvar *default-compilation-file* "Makefile")

(defun smart-compile ()
  (interactive)
  (save-buffer)
  (if eldoc-mode
      (progn (eval-buffer)
             (when (fboundp 'elk-test-run-all-buffers)
               (elk-test-run-all-buffers t)))
    (when (and (string-match  "make" compile-command)
               (null (nearest-compilation-file default-directory
                                               *default-compilation-file*)))
      (message "No Makefile found, switching to rake")
      (set (make-local-variable 'compile-command) "rake"))
    (compile compile-command)))

(defun nearest-compilation-file (dir compilation-file)
  "Search for the compilation file traversing up the directory tree."
  (let ((file-path (concat dir compilation-file))
        (parent (file-name-directory (directory-file-name dir))))
    (cond
     ((file-exists-p file-path) file-path)
     ((string= dir parent) nil)
     (t (nearest-compilation-file parent compilation-file)))))

(defun reach-compilation-file ()
  "If your compile command containts 'make', goes up in the path until it finds a makefile.

I use it like this:
    (add-hook 'compilation-mode-hook '(lambda ()
                                        (require 'reach-compilation-file)))"
  (when (string-match "make"
                      (car compilation-arguments))
    (let ((compilation-file (nearest-compilation-file (expand-file-name default-directory)
                                                      *default-compilation-file*)))
      (when (null compilation-file)
        (error "No file named '%s' found" *default-compilation-file*))
      (setq default-directory (file-name-directory compilation-file)))))

(setq compilation-process-setup-function 'reach-compilation-file)

(provide 'smart-compile)
