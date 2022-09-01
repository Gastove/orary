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
  ;; :bind  (:map lsp-language-map
  ;;       ("r i" . #'lsp-java-add-import)
  ;;       ("r t" . #'lsp-java-add-throws)
  ;;       ("r o" . #'lsp-java-organize-imports)
  ;;       ("g o" . #'lsp-java-generate-overrides)
  ;;       ("g e" . #'lsp-java-generate-equals-and-hash-code)
  ;;       ("g g" . #'lsp-java-generate-getters-and-setters)
  ;;       ("g t" . #'lsp-java-generate-to-string))
  )

(add-hook 'java-mode-hook
          (lambda ()
            (lsp)
            (subword-mode +1)
            (setq-local lsp-semantic-tokens-enable nil)
            (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.2.0/jdt-language-server-1.2.0-202106301459.tar.gz")
            (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx16G" "-Xms100m" ) ;; "-noverify" "-Xmx16G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"
                  lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                                                           :path "/usr/lib/jvm/java-1.8.0-openjdk/"
                                                           ;; :path "/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.332.b09-1.fc36.x86_64/"
                                                           )
                                                    (:name "OpenJDK-11"
                                                           :path "/usr/lib/jvm/java-11-openjdk/"
                                                           :default t)]
                  )
            ))

(provide 'orary-java)
;;; orary-java.el ends here
