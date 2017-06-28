;;; orary-text.el --- Text configuration for Orary
;;
;;; Commentary:
;; Does what it says on the tin.
;;; Code:

;; Er.

(defun orary/text-mode-defaults ()
  (toggle-word-wrap +1)
  (toggle-truncate-lines +1))

(use-package markdown-mode
  :config
  (defun orary/markdown-preview-file ()
    "Run Marked on the current file and revert the buffer."
    (interactive)
    (shell-command
     (format "open -a '/Applications/Marked 2.app' %s"
             (shell-quote-argument (buffer-file-name)))))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'". gfm-mode))
  :bind (("C-c m" . orary/markdown-preview-file)))


;; TODO: This is a good idea. Get this cleaned up.
;; (use-package rst)

(provide 'orary-text)
;;; orary-text.el ends here
