;;; orary-clojure.el --- Clojure programming in Orary
;;
;;; Commentary:
;; love2clojure.  This gets us Clojure mode and Cider.
;;; Code:

(require 'orary-lisp)

;; clojure-mode comes with clojurescript-mode baked in
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'orary/lisp-defaults))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-hook 'clojurescript-mode-hook 'orary/lisp-defaults)
(add-hook 'clojurescript-mode-hook 'cider-mode)

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t
        cider-repl-display-help-banner nil)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'orary/interactive-lisp-defaults))

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "s-r"))))

(provide 'orary-clojure)
;;; orary-clojure.el ends here
