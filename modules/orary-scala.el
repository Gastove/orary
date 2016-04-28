;;; orary-scala.el ---
;;
;;; Commentary:
;;
;;; Code:

(use-package ensime
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(provide 'orary-scala)
;;; orary-scala.el ends here
