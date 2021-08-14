;;; orary-helm.el --- Prevasive Helm in Orary
;;
;;; Commentary:
;;
;;; Code:

;; Add-ons to Helm
(use-package helm-descbinds)
(use-package helm-ag
  :init
  (custom-set-variables
   '(helm-ag-base-command "rg --no-heading")
   `(helm-ag-success-exit-status '(0 2))
   ))

(defun orary/helm-do-rg-in-project ()
  "`helm-projectile-ag' tries to compute an extra set of file
extensions to ignore, which it will pass with --ignore=ext.*.
Unfortunately, this breaks helm-ag if you're using ripgrep.
Fortunately, the function isn't hard to redefine; the below is
all it's actually doing, in the normative case, once you strip
out the extra ignore flim-flam."
  (interactive)
  (helm-do-ag (projectile-project-root) nil))

(global-unset-key (kbd "C-x c"))

(use-package helm
  :demand t
  :defines helm-command-prefix
  :diminish helm-mode
  :config
  (require 'helm-config)
  (require 'helm-projectile)
  (helm-mode 1)
  (setq  helm-display-function                  'pop-to-buffer
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
  (helm-descbinds-mode)
  :bind-keymap ("C-c h" . helm-command-prefix)
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

;; (global-set-key (kbd "C-c h") 'helm-command-prefix)

(provide 'orary-helm)
;;; orary-helm.el ends here
