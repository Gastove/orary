;;; orary-rust.el --- Rust support for Orary
;;
;;; Commentary:
;; Maybe rust is nice? I'm unlikely to be able to tell if my editor isn't set up for it ;P
;;; Code:

(require 'orary-braces)
(require 'orary-functions)

;; (use-package cargo)
;; Build tool wrapper
;; NOTE[rdonaldson|2023-04-29] I *think* LSP fully replaces racer.
;;
;; (use-package racer)         ;; Symbol completion, introspection
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
    ;; NOTE[rdonaldson|2020-05-31] with LSP mode, it's often more ergonomic to
    ;; use an lsp action to fill in match arms. Reconsidering this; might gate
    ;; it behind C-u.
    ;; (yas-expand-snippet "$1 => $0,")
    )

   ;; We're defining an if/else, while, function, struct, enum, unsafe, or trait; insert {}, then open
   ((looking-back "if.*\\|else.*\\|while.*\\|fn .*\\|\\<impl\\>.*\\|trait.*\\|struct.*\\|enum.*\\|mod.*\\|for.*\\|unsafe.*\\|async.*" (line-beginning-position))
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

(defun orary/rust-insert-arrow ()
  (interactive)
  (if (not (nth 4 (syntax-ppss)))
      (progn
       (orary/insert-key-seq "-" ">" "<")
       (set-transient-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "-") #'orary/rust-insert-arrow)
          map)))
    (insert "-")))

;; The Business
(use-package rustic
  :config
  (setq ;; NOTE[rdonaldson|2023-04-23] Dear future Ross: you are gonna think
        ;; this is a good idea, but you are wrong. With LSP, there's some
        ;; interaction between LSP and rust-analyzer and format on save that
        ;; ruins *everything*. Don't turn this back on until you get that sorted
        ;; out.
        ;;
        ;; rust-format-on-save t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        )
  (add-hook 'rustic-mode-hook
            (lambda ()
              (cargo-minor-mode +1)
              (subword-mode +1)
              (lsp-deferred)
              (flycheck-add-next-checker 'lsp 'rust-clippy)
              (setq comment-start "//")))
  
  :bind (:map rustic-mode-map
              ("<C-return>" . #'orary/rust-ret-dwim)
              ("C-o" . #'orary/braces-open-newline)
              ("-" . #'orary/rust-insert-arrow))
  )

(provide 'orary-rust)
;;; orary-rust.el ends here
