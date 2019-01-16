;;; orary-purescript.el ---
;;
;;; Commentary:
;;
;;; Code:

;; (use-package psc-ide)

;; PureScript
(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  (add-hook 'purescript-mode-hook
            (lambda ()
              (push '("->" . ?→) prettify-symbols-alist)
              (push '("=>" . ?⇒) prettify-symbols-alist)
              ;; (psc-ide-mode)
              (turn-on-purescript-indentation)
              )))

(provide 'orary-purescript)
;;; orary-purescript.el ends here
