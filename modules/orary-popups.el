;;; orary-popups.el --- Maybe I wanna use Shackle?
;;
;;; Commentary:
;;
;;; Code:

(use-package shackle
  :demand t
  :config
  (setq shackle-select-reused-windows t
        shackle-rules '((compilation-mode :noselect t)
                        (help-mode :select t)
                        ("*Gofmt Errors*" :select t)
                        (".*/\\.git/COMMIT_EDITMSG$" :regexp t :select t))
        shackle-default-rule '(:select t))
  (add-hook 'orary/programming-mode-hook (lambda () (shackle-mode +1))))

(provide 'orary-popups)
;;; orary-popups.el ends here
