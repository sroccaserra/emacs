;;
;; coding-dojo.el
;;

(defvar *dojo-template-dir* "~/Dropbox/.emacs.d/coding-dojo")
(defvar *dojo-project-dir* "~/Dropbox/Developer/CodingDojo")

(defvar *dojo-find-command* "find %s -type f")
(defvar *dojo-prune-paths* '("*/.git/*" "*/.hg/*"))

(defvar *dojo-after-new-project-command* "git init && git add . && git commit -m 'first'")

(defstruct dojo-project
  name
  language
  dir
  main-file)

(defun dojo-template-dir (language)
  (concat *dojo-template-dir* "/" language))

(defun dojo-project-dir-for (project)
  (format "%s/%s-%s"
          *dojo-project-dir*
          (upcase-initials (dojo-project-name project))
          (dojo-project-language project)))

(defun dojo-create-project (language project-name)
  (let* ((project (make-dojo-project :language (upcase-initials language)
                                     :name project-name))
         (project-dir (dojo-project-dir-for project)))
    (setf (dojo-project-dir project) project-dir)
    (setf (dojo-project-main-file project) (dojo-find-main-file project))

    (make-directory project-dir t)
    (dired-copy-file-recursive (dojo-template-dir language) project-dir
                               nil nil t 'always)
    project))

(defun dojo-delete-project (project)
  (dired-delete-file (dojo-project-dir project) 'always))

(defun dojo-unexpand-home (str)
  (let ((home (shell-command-to-string "echo -n ~")))
    (replace-in-string str home "~")))

(defun dojo-find-project-files (project)
  (let* ((project-dir (dojo-project-dir project))
         (find-command (concat (format *dojo-find-command* project-dir)
                               (dojo-prune-arguments *dojo-prune-paths*)))
         (output (shell-command-to-string find-command)))
    (split-string (dojo-unexpand-home output) "\n")))

(defun dojo-find-main-file (project)
  (find-if  '(lambda (x)
               (string-match "main" x))
            (dojo-find-project-files project)))

(defun dojo-find-test-file (project)
  (let ((project-dir (dojo-project-dir project)))
    (find-if '(lambda (file)
                (string-match (format "%s.*tests?\." project-dir)
                              file))
             (dojo-find-project-files project))))

(defun dojo-prune-arguments (path-patterns)
  (reduce '(lambda (x y)
             (concat x " ! -path " (shell-quote-argument y)))
          (push "" path-patterns)))

(defun dojo-substitute-variables (project)
  (let* ((project-name (dojo-project-name project))
         (files (dojo-find-project-files project)))
    (map nil '(lambda (file)
                (save-excursion
                  (with-current-buffer (find-file-noselect file t)
                    (goto-char 0)
                    (while (search-forward-regexp "\\$main\\>" nil t)
                      (replace-match project-name nil t))
                    (save-buffer)
                    (kill-buffer))))
         files)))

(defun dojo-project-file (main-file project-name)
  (replace-regexp-in-string "main" project-name main-file))

(defun dojo-rename-main-file (project)
  (let ((main-file (dojo-find-main-file project)))
    (rename-file main-file
                 (dojo-project-file main-file (dojo-project-name project)))))

(defun dojo-find-languages ()
  (let ((find-command (format "find %s -type d -maxdepth 1 -mindepth 1"
                                                    *dojo-template-dir*)))
    (split-string (replace-in-string (shell-command-to-string find-command)
                                     ".*/\\|\n^$"
                                     "")
                  "\n")))

(defun dojo-new-project (language project-name)
  (interactive (let ((languages (map 'list 'downcase (dojo-find-languages))))
                 (list (completing-read "Language: " languages nil t)
                       (read-from-minibuffer "Project Name: "))))
  (let ((project (dojo-create-project language project-name)))
    (dojo-substitute-variables project)
    (let ((main-file (dojo-find-main-file project)))
      (dojo-rename-main-file project)
      (when *dojo-after-new-project-command*
        (save-excursion
          (find-file (dojo-project-dir project))
          (shell-command *dojo-after-new-project-command*)
          (kill-buffer)))
      (find-file (dojo-project-file main-file project-name))
      (find-file (dojo-find-test-file project)))))

(provide 'coding-dojo)
