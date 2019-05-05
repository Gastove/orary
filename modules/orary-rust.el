;;; orary-rust.el --- Rust support for Orary
;;
;;; Commentary:
;; Maybe rust is nice? I'm unlikely to be able to tell if my editor isn't set up for it ;P
;;; Code:

(require 'orary-braces)

(use-package cargo)         ;; Build tool wrapper
(use-package racer)         ;; Symbol completion, introspection
(use-package flycheck-rust) ;; Syntax checking

(defun orary/rust-ret-dwim (arg)
  (interactive "P")

  (cond
   ;; We're in a pair of {}, open them
   ((and (looking-at "}")
         (looking-back "{" (- (point) 2)))
    (orary/braces-open-pair))

   ;; We're opening a match; insert {}, open, then expand a yasnippet
   ((looking-back "match.*" (line-beginning-position))
    (sp-insert-pair "{")
    (orary/braces-open-pair)
    (yas-expand-snippet "$1 => $0,"))

   ;; We're defining an if/else, while, function, struct, enum, unsafe, or trait; insert {}, then open
   ((looking-back "if.*\\|else.*\\|while.*\\|fn .*\\|\\<impl\\>.*\\|trait.*\\|struct.*\\|enum.*\\|mod.*\\|for.*\\|unsafe.*" (line-beginning-position))
    (sp-insert-pair "{")
    (orary/braces-open-pair))

   ;; We're in a match expression
   ((looking-back "=>.*" (line-beginning-position))
    (end-of-line)
    ;; With a prefix arg, jump out of the match
    (if arg
        (progn
          (re-search-forward "}")
          (newline-and-indent))
      (progn
        (unless (looking-back "," (- (point) 1))
          (insert-char ?,))
        (newline-and-indent)
        (yas-expand-snippet "$1 => $0,"))))

   ;; Default: try to add a ; and newline
   (t
    (unless (eolp)
      (re-search-forward ")"))

    (unless (or (looking-at ";")
                (looking-back ";" (- (point) 1))
                (looking-back "^\\s-+" (line-beginning-position)))
      (insert-char ?\;))
    (end-of-line)
    (newline-and-indent))))




(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)

(use-package lsp-mode
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook
            (lambda ()
              (require 'company-lsp)
              (push 'company-lsp company-backends)
              (add-to-list 'flycheck-checkers 'lsp-ui)
              (lsp-ui-mode +1))))

;; The Business
(use-package rust-mode
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  ;; (add-hook 'rust-mode-hook #'cargo-minor-mode)
  ;; (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (cargo-minor-mode +1)
              (racer-mode +1)
              (subword-mode +1)
              (lsp)
              (setq comment-start "//")))

  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (rust-enable-format-on-save)
  :bind (:map rust-mode-map
              ("C-c C-c" . #'rust-compile)
              ("<C-return>" . #'orary/rust-ret-dwim)
              ("C-o" . #'orary/braces-open-newline))
  )

(provide 'orary-rust)
;;; orary-rust.el ends here
