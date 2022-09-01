;;; orary-formats.el --- Serialization and Configuration Format... Configurations
;;
;;; Commentary:
;; There's a whole pile of ways data can be represented in a file -- YAML,
;; JSON, HOCON, EDN, XML, and all the others I can't think of at the moment. In
;; most cases, while these formats are associated with a language of primary
;; use (e.g. JavaScript -> JSON), most of them have also gained currency in a
;; pile of other languages too (e.g. Nearly Every Language You Can Name ->
;; JSON). So let's make sure we can always read and edit these things, yeah?
;;; Code:

(require 'smartparens)

;; LaTeX
(use-package company-auctex)
(use-package auctex
  :ensure t
  :commands (latex-mode LaTeX-mode plain-tex-mode))

;; JSON
(use-package json-mode
  :mode "\\.avsc\\|\\.json\\'")

;; nXML
(require 'hideshow)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "M-F") 'hs-toggle-hiding)

(with-eval-after-load "smartparens"
  (push 'nxml-mode sp-ignore-modes-list))

(add-hook 'nxml-mode-hook
          (lambda ()
            (flyspell-mode-off)
            (setq nxml-sexp-element-flag 't
                  require-final-newline nil)
            ;; (define-key prelude-mode-map (kbd "C-c C-i") 'nxml-balanced-close-start-tag-inline)
            ))

(setq-default nxml-child-indent 4)

;; Protocol Buffers
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (highlight-indent-guides-mode)
              (add-to-list 'flycheck-checkers 'ansible-ansiblelint)
              (setq-local tab-width 2)
              )))

;; Thrift
(use-package thrift)

;; Terraform
(use-package company-terraform)
(use-package terraform-doc)
(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook (lambda () (lsp))))

;; TOML
(use-package toml-mode)

;; jsonnet
(use-package jsonnet-mode)

(provide 'orary-formats)
;;; orary-formats.el ends here
