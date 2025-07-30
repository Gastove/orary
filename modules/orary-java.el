;;; orary-java.el --- Java language configurations for Orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 's)

(defun orary/write-javadoc (signature)
  (message "Got signature:\n%s" signature)
  (message "Has keys:\n %s" (gethash "signatures" signature)))

(defun orary/lsp-java-generate-javadoc-symbol-at-point ()
  (interactive)
  (lsp-request-async "textDocument/signatureHelp"
                     (lsp--text-document-position-params)
                     #'orary/write-javadoc
                     :error-handler #'message))

(defun orary/maybe-nixify-jdtls-dir ()
  (-let [maybe-path (getenv "JDTLS_PATH")]
    (if (s-blank? maybe-path)
        lsp-java-server-install-dir
      maybe-path)))

(defun java-server-subdir-for-jar (orig &rest args)
  "Add nix subdir to `lsp-java-server-install-dir' so that the lsp test
succeeds."
  (let* ((maybe-path (getenv "JDTLS_PATH"))
         (lsp-java-server-install-dir (if (s-blank? maybe-path)
                                          lsp-java-server-install-dir
                                        (expand-file-name "./share/java/jdtls/" maybe-path))))
    (apply orig args)))

(advice-add 'lsp-java--locate-server-jar :around #'java-server-subdir-for-jar)

(use-package lsp-java
  :after lsp
  :config
  (require 'dap-java)
  (setq lsp-java-compile-null-analysis-mode "automatic"))

(add-hook 'java-mode-hook
          (lambda ()
            (lsp)
            (setq-local lsp-java-server-install-dir (orary/maybe-nixify-jdtls-dir))
            (subword-mode +1)            
            (setq
             lsp-java-jdt-ls-prefer-native-command t
             lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx16G" "-Xms100m" )
             )))

(provide 'orary-java)
;;; orary-java.el ends here
