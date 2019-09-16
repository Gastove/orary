;;; orary-java.el --- Java language configurations for Orary
;;
;;; Commentary:
;;
;;; Code:


;; (when (and lsp-eldoc-enable-signature-help (lsp--capability "signatureHelpProvider"))
;;         (cl-incf pending)
;;         (lsp-request-async
;;          "textDocument/signatureHelp"
;;          (lsp--text-document-position-params)
;;          (lambda (signature)
;;            (when (eq request-id lsp-hover-request-id)
;;              (when-let (message (and signature (lsp--signature->eldoc-message signature)))
;;                (when (or (and lsp-eldoc-prefer-signature-help (setq pending 1))
;;                          (not lsp--eldoc-saved-message))
;;                  (setq lsp--eldoc-saved-message message)))
;;              (when (zerop (cl-decf pending))
;;                (lsp--eldoc-message lsp--eldoc-saved-message))))
;;          :error-handler #'ignore))

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
  (setq lsp-log-io 't)
  (require 'dap-java)
  (add-hook 'java-mode-hook
            (lambda ()
              (lsp)
              (subword-mode +1)
              ))
  :bind (:map lsp-language-map
              ("r i" . #'lsp-java-add-import)
              ("r t" . #'lsp-java-add-throws)
              ("r o" . #'lsp-java-organize-imports)
              ("r r" . #'lsp-rename)
              ("g o" . #'lsp-java-generate-overrides)
              ("g e" . #'lsp-java-generate-equals-and-hash-code)
              ("g g" . #'lsp-java-generate-getters-and-setters)
              ("g t" . #'lsp-java-generate-to-string)))


(provide 'orary-java)
;;; orary-java.el ends here
