;;; orary-csharp.el --- C# Configuration for Orary
;;
;;; Commentary:
;;
;;; Code:

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda ()
                                (lsp)
                                (subword-mode +1)
                                )))


(provide 'orary-csharp)
;;; orary-csharp.el ends here
