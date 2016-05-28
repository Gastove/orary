;;; orary-text.el --- Text configuration for Orary
;;
;;; Commentary:
;; Does what it says on the tin.
;;; Code:

;; Er.

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'". gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'". gfm-mode)))

;; TODO: This is a good idea. Get this cleaned up.
;; (use-package rst)

(provide 'orary-text)
;;; orary-text.el ends here
