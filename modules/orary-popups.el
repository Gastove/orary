;;; orary-popups.el --- Maybe I wanna use Shackle?
;;
;;; Commentary:
;;
;;; Code:

(use-package shackle
  :hook (orary/programming-mode . shackle-mode)
  :demand t
  :config
  (setq shackle-select-reused-windows t
        shackle-rules '((compilation-mode :noselect t)
                        (help-mode :select t)
                        ("*Gofmt Errors*" :select t)
                        ("ert" :select nil)
                        ("magit-diff:.*" :noselect t :regexp t)
                        (".*[\\.git/]?COMMIT_EDITMSG" :regexp t :select t))
        shackle-default-rule '(:select t))
  )

(provide 'orary-popups)
;;; orary-popups.el ends here
