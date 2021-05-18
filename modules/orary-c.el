;;; orary-c.el --- C/++ Configs for Orary
;;
;;; Commentary:
;;
;;; Code:

(use-package ccls)

(add-hook 'cc-mode
          (lambda () (lsp)))

(add-hook 'c++-mode-hook
          (lambda () (lsp)))

(provide 'orary-c)
;;; orary-c.el ends here
