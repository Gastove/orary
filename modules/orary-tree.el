;;; orary-tree.el --- Tree-based browsing, editing, file management, etc
;;
;;; Commentary:
;;
;;; Code:

;; Neotree is unmaintained and buggy. We'll try Treemacs for a bit, see if its as nice.
;; Neotree
;; (use-package neotree
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
;;         neo-window-fixed-size t
;;         neo-window-width 100
;;         neo-window-position 'left))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-all-the-icons
  :after treemacs)

(provide 'orary-tree)
;;; orary-tree.el ends here
