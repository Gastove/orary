;;; orary-rust.el --- Rust support for Orary
;;
;;; Commentary:
;; Maybe rust is nice? I'm unlikely to be able to tell if my editor isn't set up for it ;P
;;; Code:

(use-package cargo)         ;; Build tool wrapper
(use-package racer)         ;; Symbol completion, introspection
(use-package flycheck-rust) ;; Syntax checking

(defun orary/rust-ret-dwim ()
  (interactive)
  (unless (eolp)
    (re-search-forward ")"))
  
  (unless (or (looking-at ";")
              (looking-back ";" (- (point) 1)))
    (insert-char ?\;))
  (end-of-line)
  (newline-and-indent))

;; The Business
(use-package rust-mode
  :config
  (setq racer-cmd "~/.cargo/bin/racer") 
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (rust-enable-format-on-save)
  :bind (:map rust-mode-map
              ("C-c n"   . #'rust-format-buffer)
              ("C-c C-c" . #'rust-compile)
              ("<C-return>" . #'orary/rust-ret-dwim))
  )

(provide 'orary-rust)
;;; orary-rust.el ends here
