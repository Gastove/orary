;;; orary-java.el --- Java language configurations for Orary
;;
;;; Commentary:
;;
;;; Code:

(defun orary/write-javadoc (signature)
  (message "Got signature:\n%s" signature)
  (message "Has keys:\n %s" (gethash "signatures" signature)))

(defun orary/lsp-java-generate-javadoc-symbol-at-point ()
  (interactive)
  (lsp-request-async "textDocument/signatureHelp"
                     (lsp--text-document-position-params)
                     #'orary/write-javadoc
                     :error-handler #'message))

(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (setq lsp-log-io 't
        lsp-java-vmargs '("-noverify" "-Xmx4G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"))
  (require 'dap-java)
  :bind (:map lsp-language-map
              ("r i" . #'lsp-java-add-import)
              ("r t" . #'lsp-java-add-throws)
              ("r o" . #'lsp-java-organize-imports)
              ("g o" . #'lsp-java-generate-overrides)
              ("g e" . #'lsp-java-generate-equals-and-hash-code)
              ("g g" . #'lsp-java-generate-getters-and-setters)
              ("g t" . #'lsp-java-generate-to-string)))

(add-hook 'java-mode-hook
          (lambda ()
            (lsp)
            (subword-mode +1)
            ))

(provide 'orary-java)
;;; orary-java.el ends here
