;;; orary-rust.el --- Rust support for Orary
;;
;;; Commentary:
;; Maybe rust is nice? I'm unlikely to be able to tell if my editor isn't set up for it ;P
;;; Code:

(use-package cargo)         ;; Build tool wrapper
(use-package racer)         ;; Symbol completion, introspection
(use-package flycheck-rust) ;; Syntax checking


;; The Business
(use-package rust-mode
  :config
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  ;; For now, this is a no-go; rustfmt is deprecated and requires a `--force'
  ;; flag to run, but the new rustfmt-nightly is missing dylibs I can't provide.
  ;; Cool.
  ;; :bind (:map rust-mode-map
  ;;             ("C-c n" . #'rust-format-buffer))
  )

(provide 'orary-rust)
;;; orary-rust.el ends here
