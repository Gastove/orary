;;; orary-java.el --- Java language configurations for Orary
;;
;;; Commentary:
;;
;;; Code:

(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (require 'dap-java)
  (add-hook 'java-mode-hook
            (lambda ()
              (lsp)
              )))

(provide 'orary-java)
;;; orary-java.el ends here
