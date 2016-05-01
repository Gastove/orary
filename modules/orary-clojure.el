;;; orary-clojure.el --- Clojure programming in Orary
;;
;;; Commentary:
;; love2clojure.  This gets us Clojure mode and Cider.
;;; Code:

(require 'orary-lisp)

(use-package clojure-mode
  :mode "\\.cljs?\\'"  
  :config
  (add-hook 'clojure-mode-hook 'orary/lisp-defaults)
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t)

  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'orary/interactive-lisp-defaults))

(provide 'orary-clojure)
;;; orary-clojure.el ends here
