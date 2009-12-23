(defvar *compilation-file* "Makefile")

(defun reach-compilation-file ()
  (when (string-match "make"
                      (car compilation-arguments))
    ;; Search for the compilation file traversing up the directory tree.
    (let* ((dir (expand-file-name default-directory))
           (parent (file-name-directory (directory-file-name dir))))
      (while (and (not (file-readable-p (concat dir *compilation-file*)))
                  (not (string= parent dir)))
        (setq dir parent
              parent (file-name-directory (directory-file-name dir))))
      (if (string= dir parent)
          (error "Search file %s is missing" *compilation-file*)
        (with-current-buffer compilation-last-buffer
          (setq default-directory dir))))))

(setq compilation-process-setup-function 'reach-compilation-file)

(provide 'reach-compilation-file)
