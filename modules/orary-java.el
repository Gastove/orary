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
  :after lsp
  :config
  (require 'dap-java)
  (setq lsp-java-compile-null-analysis-mode "automatic"))

(add-hook 'java-mode-hook
          (lambda ()
            (lsp)
            (subword-mode +1)            
            (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx16G" "-Xms100m" )
                  )))

(provide 'orary-java)
;;; orary-java.el ends here
