;;; orary-lisp.el --- Lisp Configs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Configurations common across Common Lisp, Emacs Lisp, Clojure, et al.
;;; Code:

;;; Languages
;; Lisp

(require 'diminish)

(defun orary/lisp-defaults ()
  "Set default values for lisp-language major modes."
  (eldoc-mode +1)
  (subword-mode +1)
  (diminish 'subword-mode)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1)
  (push '("lambda" . ?λ) prettify-symbols-alist))

(defun orary/interactive-lisp-defaults ()
  (subword-mode +1)
  (diminish 'subword-mode)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1))

;; NOTE[rdonaldson|2023-04-29] I had a thought Geiser was fucking up xref in LSP
;; mode, but I *think* it was actually lisp hooks for smartparens. Remember this
;; is here in case something else breaks! Scheme
(use-package geiser
  :config
  (add-hook 'scheme-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'scheme-mode-hook #'orary/lisp-defaults)
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename
        (expand-file-name "geiser-history" orary/save-root)))

(provide 'orary-lisp)
;;; orary-lisp.el ends here
