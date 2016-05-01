;;; orary-osx.el --- When We're On OSX, Some Settings Need Set
;;
;;; Commentary:
;; Modifiers and the path, mostly.
;;; Code:


;; The Path
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; OSX Modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


(provide 'orary-osx)
;;; orary-osx.el ends here
