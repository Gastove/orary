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

;; Hrm: \\|^type [A-Za-z'_]+ =
(defun fsharp-ret-dwim ()
  "Look for an operator in the currently line -- one of |, |>,
||>, |||>. Insert a newline after current line, indent, and
insert the found character. Note: this function currently does
not support types/classes, even though it would be consistent
from a usability standpoint to do so."
  (interactive)
  (let ((search-bol (line-beginning-position))
        (search-eol (line-end-position))
        insertion-char
        insertion-tpl)
    (beginning-of-line)
    (if (re-search-forward "\\(|\\{1,3\\}>?\\)" search-eol)
        (setq insertion-char (match-string 1)))
    (end-of-line)
    (newline-and-indent)
    (when insertion-char
      (insert insertion-char)
      (insert " ")
      (when (string= insertion-char "|")
        (save-excursion
          (cond
           ;; We're in a match block
           ((re-search-backward "->" search-bol t) (setq insertion-tpl "$1 -> $0"))
           ;; We're in a discriminated union
           ((re-search-backward "of" search-bol t) (setq insertion-tpl "$1 of $0"))))
        (yas-expand-snippet insertion-tpl)))))

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
  (add-hook 'fsharp-mode-hook (lambda () (setq dabbrev-check-all-buffers nil)))
  :bind (:map fsharp-mode-map
              ("<C-return>" . fsharp-ret-dwim)))

(provide 'orary-fsharp)
;;; orary-fsharp.el ends here
