;;; orary-smartparens.el --- Smartparens
;;
;;; Commentary:
;;
;;; Code:

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  ;; (sp-use-paredit-bindings)
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (setq sp-ignore-modes-list
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))

  ;; Org-mode Pairs
  (sp-local-pair 'org-mode "~" "~" :wrap "C-~")
  (sp-local-pair 'org-mode "*" "*" :wrap "C-*")
  (sp-local-pair 'sql-mode "<" ">")
  (sp-local-pair 'java-mode "/**" "**/")
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-M-u" . sp-up-sexp)
              ("C-M-d" . sp-down-sexp)

              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-S-u" . sp-backward-up-sexp)
              ("C-S-d" . sp-backward-down-sexp)

              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-s" . sp-splice-sexp)
              ;; ("M-<delete>" . sp-unwrap-sexp)
              ;; ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-S-<backspace>" . sp-splice-sexp-killing-around)
              ("C-]" . sp-select-next-thing-exchange)
              ("C-M-]" . sp-select-next-thing)
              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol)
              ))

(provide 'orary-smartparens)
;;; orary-smartparens.el ends here
