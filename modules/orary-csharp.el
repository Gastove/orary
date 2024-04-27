;;; orary-csharp.el --- C# Configuration for Orary
;;
;;; Commentary:
;;
;;; Code:
(setq lsp-csharp-csharpls-use-dotnet-tool t
      lsp-csharp-csharpls-use-local-tool t)

(add-hook 'csharp-mode-hook (lambda ()
                              (lsp)
                              (subword-mode +1)
                              ))
(provide 'orary-csharp)
;;; orary-csharp.el ends here
