(defvar *jekyll-post-dir* "~/Developer/sroccaserra.github.com/_posts/")

(defun jekyll:new-post (title)
  (interactive "sTitle: ")
  (let ((dashed-title (replace-in-string (downcase title) " " "-"))
        (header (format "---\ntitle: %s\nlayout: post\n---\n\n{{ page.title }}\n================\n\n"
                        title)))
    (find-file (concat *jekyll-post-dir*
                       (format-time-string "%Y-%m-%d-")
                       dashed-title
                       ".md"))
    (insert header)
    (viper-insert nil)))

(provide 'jekyll)
