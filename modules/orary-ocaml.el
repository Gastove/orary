;;; orary-ocaml.el --- OCaml configuration for Orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:



;; (use-package merlin
;;   :load-path "~/.opam/default/share/emacs/site-lisp"
;;   )

(use-package tuareg
  ;; (push "<SHARE_DIR>/emacs/site-lisp" load-path) ; directory containing merlin.el
  ;; (setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
  ;; (autoload 'merlin-mode "merlin" "Merlin mode" t)
  :config
  ;; (add-hook 'tuareg-mode-hook 'merlin-mode)
  ;; (add-hook 'caml-mode-hook 'merlin-mode)
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (lsp)
              ))
  )

(provide 'orary-ocaml)
;;; orary-ocaml.el ends here
