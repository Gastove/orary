;;; orary-misc.el --- Small language configs
;;
;;; Commentary:
;; Sometimes I try out a language. Sometimes I don't know if it needs its own
;; module. This is for those things.
;;; Code:

(require 'orary-lisp)

(use-package nim-mode
  :mode "\\.nim\\'"
  :config
  (require 'company-nim)
  (require 'flycheck-nim-async)
  (add-to-list 'company-backends '(company-nim :with company-nim-builtin)))

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$")

(use-package rust-mode)

;; Scheme
(use-package geiser
  :config
  (add-hook 'scheme-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'scheme-mode-hook #'orary/lisp-defaults)
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename
        (expand-file-name "geiser-history" orary/save-root)))

(use-package haskell-mode)

(provide 'orary-misc)
;;; orary-misc.el ends here
