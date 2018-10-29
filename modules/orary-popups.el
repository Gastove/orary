;;; orary-popups.el --- Maybe I wanna use Shackle?
;;
;;; Commentary:
;;
;;; Code:

(use-package shackle
  :config
  (setq shackle-rules '((compilation-mode :noselect t)
                        ("*Gofmt Errors*" :select t))
        shackle-default-rule '(:select t)))

(provide 'orary-popups)
;;; orary-popups.el ends here
