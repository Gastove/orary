;;; orary-purescript.el ---
;;
;;; Commentary:
;;
;;; Code:

;; PureScript
(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  (add-hook 'purescript-mode-hook
	    (lambda ()
	      (push '("->" . ?→) prettify-symbols-alist)
	      (push '("=>" . ?⇒) prettify-symbols-alist)
	      )))

(provide 'orary-purescript)
;;; orary-purescript.el ends here
