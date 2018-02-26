;;; orary-helm.el --- Prevasive Helm in Orary
;;
;;; Commentary:
;;
;;; Code:

;; Add-ons to Helm
(use-package helm-descbinds)
(use-package helm-ag :demand t)
(use-package helm-projectile)

(use-package helm
  :demand t
  :commands helm
  :diminish helm-mode
  :config
  (require 'helm)
  (require 'helm-config)
  (require 'helm-projectile)
  (helm-mode 1)
  (setq helm-split-window-in-side-p            t
        helm-split-window-default-side         'below
        helm-move-to-line-cycle-in-source      t
        helm-ff-search-library-in-sexp         t
        helm-ff-file-name-history-use-recentf  t
        ;; Fuzzy Matching
        helm-buffers-fuzzy-matching            t
        helm-M-x-fuzzy-match                   t
        helm-recentf-fuzzy-match               t
        helm-ff-skip-boring-files              t)
  (add-to-list 'helm-boring-file-regexp-list "\\.py[oc]$")
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq projectile-completion-system 'helm
        helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
  (helm-projectile-on)
  (helm-descbinds-mode)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :bind (("M-x"      . helm-M-x)
         ("M-y"      . helm-show-kill-ring)
         ("C-x b"    . helm-mini)
         ("C-c f"    . helm-recentf)
         ("C-x C-f"  . helm-find-files)
         ("C-h f"    . helm-apropos)
         :map helm-map
         ([tab]      . helm-execute-persistent-action)
         ("C-i"      . helm-execute-persistent-action)
         ("C-z"      . helm-select-action)
         :map helm-command-map
         ("o"        . helm-occur)
         ("g"        . helm-do-grep)
         ("SPC"      . helm-all-mark-rings)))

(provide 'orary-helm)
;;; orary-helm.el ends here
