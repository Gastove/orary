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

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;; nXML
(with-eval-after-load "smartparens"
 (push 'nxml-mode sp-ignore-modes-list))

(add-hook 'nxml-mode-hook
          (lambda ()
            (flyspell-mode-off)
            ;; (define-key prelude-mode-map (kbd "C-c C-i") 'nxml-balanced-close-start-tag-inline)
            ))

(setq-default nxml-child-indent 4)

;; Protocol Buffers
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.yaml\\'")

(provide 'orary-formats)
;;; orary-formats.el ends here
