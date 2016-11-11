;;; orary-fsharp.el --- F# language configs for orary
;;
;;; Commentary:
;; F# mode configuration, smartparens pairs.
;;; Code:

(require 'smartparens)
(require 'dash)
(require 'dabbrev)

(defun orary/sp-insert-pipe-p (delim action ctxt)
  (save-excursion
    (and (eq ctxt 'code)
         (not (bolp))
         (looking-back "[(\\[]|" 2))))

(defun orary/sp-insert-at-p (delim action ctxt)
  (save-excursion
    (and (eq ctxt 'code)
         (not (bolp))
         (looking-back "<\\|<@\\{1,2\\}" 2))))

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$"
  :config
  (add-hook 'fsharp-mode-hook
            (lambda ()
              (subword-mode +1)
              (setq company-auto-complete nil)))
  (sp-local-pair 'fsharp-mode "<" ">")
  (sp-local-pair 'fsharp-mode "@" "@"
                 :actions '(insert wrap navigate)
                 :when '(orary/sp-insert-at-p))
  (sp-local-pair 'fsharp-mode "|" "|" :when '(orary/sp-insert-pipe-p))
  (add-hook 'fsharp-mode-hook (lambda () (setq dabbrev-check-all-buffers nil))))

(provide 'orary-fsharp)
;;; orary-fsharp.el ends here
