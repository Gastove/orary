;;; orary-helm.el --- Prevasive Helm in Orary
;;
;;; Commentary:
;;
;;; Code:

(use-package helm
  :demand t
  :commands helm
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p           t
	helm-split-window-default-side        'below
	helm-split-window-in-side-p           t
	helm-buffers-fuzzy-matching           t
	helm-move-to-line-cycle-in-source     t
	helm-ff-search-library-in-sexp        t
	helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-c f" . helm-recentf)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("[tab]" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 :map helm-command-map
	 ("o" . helm-occur)
	 ("g" . helm-do-grep)
	 ("SPC" . helm-all-mark-rings)))

(provide 'orary-helm)
;;; orary-helm.el ends here