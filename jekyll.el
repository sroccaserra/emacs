(defvar *jekyll-post-dir* "~/Developer/github/sroccaserra.github.com/_posts/")

(defun jekyll:new-post(title)
  (interactive "sTitle: ")
  (let ((dashed-title (replace-in-string (downcase title) " " "-"))
        (header (format "---\ntitle: %s\nlayout: default\n---\n\n"
                        title)))
    (find-file (concat *jekyll-post-dir*
                       (format-time-string "%Y-%m-%d-")
                       dashed-title
                       ".md"))
    (insert header)
    (viper-insert nil)))

(provide 'jekyll)
