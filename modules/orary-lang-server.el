;;; orary-lang-server.el --- LSP Mode integration and global configs
;;
;;; Commentary:
;; This would normally be named orary-lsp under my general conventions, but
;; there's *already* an orary-lisp, and I just wanna disambiguate this better.
;;; Code:

(use-package dap-mode
  :after lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable nil)
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(setq lsp-keymap-prefix "C-x l")

(use-package origami)
(use-package lsp-origami)

(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'lsp-mode-hook
            (lambda ()
              (dap-mode 1)
              (dap-ui-mode 1)
              (add-to-list 'flycheck-checkers 'lsp-ui)
              (lsp-ui-mode +1)
              ))

  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)
  :config
  (setq lsp-prefer-flymake nil
        lsp-prefer-capf t
        lsp-auto-guess-root t
        lsp-restart 'auto-restart
        lsp-enable-file-watchers nil
        lsp-session-file (f-expand ".lsp-session-v1" orary/save-root)
        lsp-enable-semantic-highlighting t)

  :bind (:map lsp-mode-map
              ("M-." . #'lsp-ui-peek-find-definitions)
              ("M-?"  . #'lsp-ui-peek-find-references)
              ))

(provide 'orary-lang-server)
;;; orary-lang-server.el ends here
