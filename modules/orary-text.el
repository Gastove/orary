;;; orary-text.el --- Text configuration for Orary
;;
;;; Commentary:
;; Does what it says on the tin.
;;; Code:

;; Er.

(defun orary/text-mode-defaults ()
  (abbrev-mode +1)
  (hl-line-mode +1)
  (toggle-word-wrap +1)
  (toggle-truncate-lines -1))

(add-hook 'text-mode-hook #'orary/text-mode-defaults)

(use-package markdown-mode
  :config
  (defun orary/markdown-preview-file ()
    "Run Marked on the current file and revert the buffer."
    (interactive)
    (if (eq system-type 'darwin)
        (shell-command
         (format "open -a '/Applications/Marked 2.app' %s"
                 (shell-quote-argument (buffer-file-name))))
      (message "Can't preview markdown on non-OSX machines")))

  (add-to-list 'auto-mode-alist '("\\.markdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'". gfm-mode))

  (setq markdown-command "pandoc")

  :bind (:map markdown-mode-map
              ("C-c m" . orary/markdown-preview-file)))
;; TODO: This is a good idea. Get this cleaned up.
;; (use-package rst)

(provide 'orary-text)
;;; orary-text.el ends here
