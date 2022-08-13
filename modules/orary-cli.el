;;; orary-cli.el --- Functions from the Terminal
;;
;;; Commentary:
;; I still use my terminal a great deal; being able to hook in to Emacs more
;; easily is good news.
;;; Code:

(defun orary/projectile-add-cwd ()
  "Adds `default-directory' as a projectile known project.
Expects to be called from the terminal, in the root directory of
a project."
  (-let [dir (f-short default-directory)]
    (unless (-contains? projectile-known-projects dir)
      (projectile-add-known-project dir))))

(provide 'orary-cli)
;;; orary-cli.el ends here
