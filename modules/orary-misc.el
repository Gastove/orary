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

;; Misc languages whee
(use-package haskell-mode)
(use-package dockerfile-mode)
(use-package rust-mode)
(use-package coffee-mode)
(use-package groovy-mode
  :mode "\\.gradle\\'")
(use-package php-mode)

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



(add-to-list 'auto-mode-alist '("\\.hql\\'" . sql-mode))

(provide 'orary-misc)
;;; orary-misc.el ends here
