;;; orary-csharp.el --- C# Configuration for Orary
;;
;;; Commentary:
;;
;;; Code:

(setq lsp-csharp-csharpls-use-dotnet-tool nil
      lsp-csharp-csharpls-use-local-tool nil)

(add-hook 'csharp-mode-hook (lambda ()
                                (lsp)
                                (subword-mode +1)
                                ))

(provide 'orary-csharp)
;;; orary-csharp.el ends here
