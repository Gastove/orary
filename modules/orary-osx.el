;;; orary-osx.el --- When We're On OSX, Some Settings Need Set
;;
;;; Commentary:
;; Modifiers and the path, mostly.
;;; Code:


;; The Path
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "WORKON_HOME")

;; OSX Modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Use GNU `ls' instead of fucked-up custom Mac ls
(setq insert-directory-program "gls")

(provide 'orary-osx)
;;; orary-osx.el ends here
