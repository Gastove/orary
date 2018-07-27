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

(defun orary/sp-close-open-angle-p (delim action ctxt)
  (save-excursion
    (looking-back "[^|-]" 2)))

(defun orary/sp-correct-angle-bracket (delim action ctxt)
  (save-excursion
    (when (looking-at-p ">@")
      (delete-char 2)
      (insert "@>"))))

(defun fsharp-ret-dwim ()
  "Look for an operator in the currently line -- one of |, |>,
||>, |||>. Insert a newline after current line, indent, and
insert the found character, along with a proper yasnippet for
types and match blocks. Note: this function currently does not
support classes, even though it would be consistent from a
usability standpoint to do so."
  (interactive)
  (let ((search-bol (line-beginning-position))
        (search-eol (line-end-position))
        (type-yas-tpl "$1 of $0")
        (match-yas-tpl "$1 -> $0")
        insertion-char
        insertion-tpl)
    (beginning-of-line)
    (cond
     ;; Discriminated union types
     ((looking-at "type\\|\\s-*|.*of")
      (setq insertion-char "|"
            insertion-tpl type-yas-tpl))
     ;; Match blocks
     ((looking-at "\\s-*match .* with\\|\\s-*| .*->")
      (setq insertion-char "|"
            insertion-tpl match-yas-tpl))
     ;; Arbitrary piping expressions
     ((looking-at "\\s-*\\(|\\{1,3\\}>\\)")
      (setq insertion-char (match-string 1))))
    (end-of-line)
    (newline-and-indent)
    (when insertion-char
      (insert insertion-char)
      (insert " ")
      (when insertion-tpl
        (yas-expand-snippet insertion-tpl)))))

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$"
  :config
  (add-hook 'fsharp-mode-hook
            (lambda ()
              (subword-mode +1)
              (setq company-auto-complete nil
                    require-final-newline nil)))
  (sp-with-modes 'fsharp-mode
    (sp-local-pair "<" ">"
                   :when '((orary/sp-close-open-angle-p))
                   :post-handlers '(orary/sp-correct-angle-bracket))
    (sp-local-pair "@" "@"
                   :actions '(insert wrap navigate)
                   :when '(orary/sp-insert-at-p))
    (sp-local-pair "|" "|" :when '(orary/sp-insert-pipe-p))
    (sp-local-pair "\"\"\"" "\"\"\"")
    (sp-local-pair "`" "`" :actions :rem)
    (sp-local-pair "``" "``"))
  (add-hook 'fsharp-mode-hook (lambda () (setq dabbrev-check-all-buffers nil)))
  :bind (:map fsharp-mode-map
              ("<C-return>" . fsharp-ret-dwim)))

(provide 'orary-fsharp)
;;; orary-fsharp.el ends here
