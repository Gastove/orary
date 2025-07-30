;;; orary-common-lisp.el --- Common Lisp configs for Orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Does what it says on the tin.
;;; Code:

(require 'orary-lisp)

(use-package slime
  :config
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-fancy))
  (add-hook 'lisp-mode-hook #'orary/lisp-defaults)
  (add-hook 'slime-repl-mode-hook #'orary/interactive-lisp-defaults))

(provide 'orary-common-lisp)
;;; orary-common-lisp.el ends here
