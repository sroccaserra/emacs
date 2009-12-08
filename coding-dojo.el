;;
;; coding-dojo.el
;;

(defvar *coding-dojo-directory* "~/Dropbox/Developer/Dojo")

(defun coding-dojo:new-project (name language)
  (interactive "sProject Name: \nSLanguage: ")
  (let ((capitalized-name (upcase-initials name)))
  (let ((path (coding-dojo:path capitalized-name language)))
    (if (file-exists-p path)
        (message "Error: directory '%s' already exists." path)
      (coding-dojo:make-project capitalized-name language)))))

(defun coding-dojo:path (name language)
  (concat *coding-dojo-directory* "/"
          name "-" (capitalize (symbol-name language))))

(defun coding-dojo:configuration (language)
  (cdr (assoc language *coding-dojo:configurations*)))

(defun coding-dojo:get (token language)
  (let ((configuration (coding-dojo:configuration language)))
    (cdr (assoc token configuration))))

(defun coding-dojo:main-text (language project-name)
  (let* ((extension (coding-dojo:get 'extension language))
         (main-file (concat project-name extension)))
  (format (coding-dojo:get 'main-text language) main-file project-name)))

(defun coding-dojo:test-text (language name)
  (let ((test-file (coding-dojo:get 'test-file language)))
    (format (coding-dojo:get 'test-text language) test-file name)))

(defun coding-dojo:make-text (language)
  (let ((test-file (coding-dojo:get 'test-file language))
        (test-dir (coding-dojo:get 'test-dir language)))
    (when test-dir
      (setq test-file (concat test-dir "/" test-file)))
    (format (coding-dojo:get 'make-text language) test-file)))

(defun coding-dojo:make-project (name language)
  (let ((path (coding-dojo:path name language))
        (src-dir (coding-dojo:get 'src-dir language))
        (test-dir (coding-dojo:get 'test-dir language))
        (main-file (concat name (coding-dojo:get 'extension language)))
        (test-file (coding-dojo:get 'test-file language))
        (make-file (coding-dojo:get 'make-file language)))
    (make-directory path t)
    (when make-file
      ;; Make file
      (find-file (concat path "/" make-file))
      (insert (coding-dojo:make-text language))
      (basic-save-buffer))
    ;; Main file
    (when src-dir
      (make-directory (concat path "/" src-dir))
      (setq main-file (concat src-dir "/" main-file)))
    (find-file (concat path "/" main-file))
    (insert (coding-dojo:main-text language name))
    (basic-save-buffer)
    ;; Test file
    (when test-dir
      (make-directory (concat path "/" test-dir))
      (setq test-file (concat test-dir "/" test-file)))
    (find-file (concat path "/" test-file))
    (insert (coding-dojo:test-text language name))
    (basic-save-buffer)
    ;; Final actions
    (viper-insert nil)
    (shell-command "hg init && hg add * && hg commit -m first")
    (map nil
         '(lambda (file)
            "Set VC mode in every file"
            (find-file (concat path "/" file))
            (vc-find-file-hook))
         (list make-file main-file test-file))
    (when make-file
      (compile compile-command))))

;; Config

(setq *coding-dojo:configurations*
  '((lua . ((extension . ".lua")
            (main-text . "--\n-- %s\n--\n\n")
            (test-file . "Tests.lua")
            (test-text . "--\n-- %s\n--\nrequire 'luaunit'\nrequire '%s'\n\nmodule(..., package.seeall)\n\n")
            (make-file . "Makefile")
            (make-text . "test:\n\tlua -e \"require 'Tests';LuaUnit:run();os.exit(math.min(UnitResult.failureCount, 255))\"\n")))

    (elisp . ((extension . ".el")
              (main-text . ";;\n;; %s\n;;\n\n")
              (test-file . "Tests.elk")
              (test-text . ";;\n;; %s\n;;\n\n")))

    (python . ((extension .".py")
               (main-text . "#\n# %s\n#\n\n")
               (test-file . "Tests.py")
               (test-text . "#\n# %s\n#\n\nimport unittest\nfrom %s import *\n\nclass Tests(unittest.TestCase):\n    pass\n\nif __name__ == '__main__':\n    unittest.main()")
               (make-file . "Makefile")
               (make-text . "test:\n\tpython %s")))

    (ruby . ((extension . ".rb")
             (main-text . "#\n# %s\n#\n\n")
             (test-file . "Tests.rb")
             (test-text . "#\n# %s\n#\n\nrequire 'test/unit'\nrequire '%s'\n\nclass Tests < Test::Unit::TestCase\nend\n")
             (make-file . "Rakefile")
             (make-text . "task :default do ruby '%s' end")))

    (haskell . ((extension . ".hs")
                (main-text . "--\n-- %s\n--\nmodule %s where\n\n")
                (test-file . "Tests.hs")
                (test-text . "--\n-- %s\n--\n\nmodule Main where\nimport Test.HUnit\nimport %s\n\n\nmain = runTestTT $\n       []")
                (make-file . "Makefile")
                (make-text . "test:\n\trunghc %s")))))

(provide 'coding-dojo)
