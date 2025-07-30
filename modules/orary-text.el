;;; orary-text.el --- Text configuration for Orary -*- lexical-binding: t; -*-
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
    (cond ((eq system-type 'darwin)
           (shell-command
            (format "open -a '/Applications/Marked 2.app' %s"
                    (shell-quote-argument (buffer-file-name)))))
          ((and (eq system-type 'gnu/linux) (executable-find "typora"))
           ;; TODO: store the process object; name it appropriately; find a way to cancel it.
           (start-process-shell-command "typora" "*typora*"
                                        (format "typora %s"
                                                (shell-quote-argument (buffer-file-name)))))
          ('t (message "Can't preview markdown on non-OSX machines"))))

  (add-to-list 'auto-mode-alist '("\\.markdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'". gfm-mode))

  (setq markdown-command "pandoc")

  ;; Honestly, auto-fill-mode is annoying the heck out of me.
  ;; (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode +1)))

  :bind (:map markdown-mode-map
              ("C-c m" . orary/markdown-preview-file)))
;; TODO: This is a good idea. Get this cleaned up.
;; (use-package rst)

(provide 'orary-text)
;;; orary-text.el ends here
