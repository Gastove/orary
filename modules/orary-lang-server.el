;;; orary-lang-server.el --- LSP Mode integration and global configs
;;
;;; Commentary:
;; This would normally be named orary-lsp under my general conventions, but
;; there's *already* an orary-lisp, and I just wanna disambiguate this better.
;;; Code:

(use-package dap-mode)

(use-package lsp-ui
  :commands
  lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-restart 'auto-restart)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (dap-mode 1)
              (dap-ui-mode 1)
              (add-to-list 'flycheck-checkers 'lsp-ui)
              (lsp-ui-mode +1)))

  :bind (:map lsp-mode-map
              ("M-." . #'lsp-ui-peek-find-definitions)
              ("M-?"  . #'lsp-ui-peek-find-references))
  )

(provide 'orary-lang-server)
;;; orary-lang-server.el ends here
