;;
;; coding-dojo.el
;;

(defvar *cd-template-dir* "~/Dropbox/.emacs.d/coding-dojo")
(defvar *cd-project-dir* "~/Dropbox/Developer/CodingDojo")

(defvar *cd-find-command* "find %s -type f")
(defvar *cd-prune-paths* '("*/.git/*" "*/.hg/*"))

(defvar *cd-after-new-project-command* "git init && git add . && git commit -m 'first'")

(defstruct cd-project
  name
  language
  dir
  extension
  main-file)

(defun cd-template-dir (language)
  (concat *cd-template-dir* "/" language))

(defun cd-project-dir-for (project)
  (format "%s/%s-%s"
          *cd-project-dir*
          (upcase-initials (cd-project-name project))
          (upcase-initials (cd-project-language project))))

(defun cd-create-project (language project-name)
  (let* ((project (make-cd-project :language language
                                   :name project-name))
         (project-dir (cd-project-dir-for project)))
    (setf (cd-project-dir project) project-dir)
    (setf (cd-project-main-file project) (cd-find-main-file project))
;    (setf (cd-project-extension project) (cd-find-extension project))

    (make-directory project-dir t)
    (dired-copy-file-recursive (cd-template-dir language) project-dir
                               nil nil t 'always)
    project))

(defun cd-delete-project (project)
  (dired-delete-file (cd-project-dir project) 'always))

(defun cd-unexpand-home (str)
  (let ((home (shell-command-to-string "echo -n ~")))
    (replace-in-string str home "~")))

(defun cd-find-project-files (project)
  (let* ((project-dir (cd-project-dir project))
         (find-command (concat (format *cd-find-command* project-dir)
                               (cd-prune-arguments *cd-prune-paths*)))
         (output (shell-command-to-string find-command)))
    (split-string (cd-unexpand-home output) "\n")))

(defun cd-find-main-file (project)
  (find-if  '(lambda (x)
               (string-match "main" x))
            (cd-find-project-files project)))

(defun cd-find-test-file (project)
  (let ((project-dir (cd-project-dir project)))
    (find-if '(lambda (file)
                (string-match (format "%s.*tests?\." project-dir)
                              file))
             (cd-find-project-files project))))

(defun cd-find-extension (project)
  (let ((main-file (cd-find-main-file project)))
    (string-match "main\\.\\(.*\\)$" main-file)
    (match-string 1 main-file)))

(defun cd-prune-arguments (path-patterns)
  (reduce '(lambda (x y)
             (concat x " ! -path " (shell-quote-argument y)))
          (push "" path-patterns)))

(defun cd-substitute-variables (project)
  (let* ((project-name (cd-project-name project))
         (files (cd-find-project-files project)))
    (map nil '(lambda (file)
                (save-excursion
                  (with-current-buffer (find-file-noselect file t)
                    (goto-char 0)
                    (while (search-forward-regexp "\\$main\\>" nil t)
                      (replace-match project-name nil t))
                    (save-buffer)
                    (kill-buffer))))
         files)))

(defun cd-project-file (main-file project-name)
  (replace-regexp-in-string "main" project-name main-file))

(defun cd-rename-main-file (project)
  (let ((main-file (cd-find-main-file project)))
    (rename-file main-file
                 (cd-project-file main-file (cd-project-name project)))))

(defun cd-new-project (language project-name)
  (interactive "sLanguage: \nsProject Name: ")
  (let ((project (cd-create-project language project-name)))
    (cd-substitute-variables project)
    (let ((main-file (cd-find-main-file project)))
      (cd-rename-main-file project)
      (when *cd-after-new-project-command*
        (save-excursion
          (find-file (cd-project-dir project))
          (shell-command *cd-after-new-project-command*)
          (kill-buffer)))
      (find-file (cd-project-file main-file project-name))
      (find-file (cd-find-test-file project)))))

(provide 'coding-dojo)
