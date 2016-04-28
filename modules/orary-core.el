;;; orary-core.el --- Core behaviors for Orary
;;
;;; Commentary:
;; Saving, custom files, core behavior.
;;; Code:

;; Remember where we were in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/savefile/saved-places")


;; Remember command and file history
(require 'savehist)
(setq savehist-file "~/.emacs.d/savefile/savehist")

;; Custom File
(setq custom-file "~/.emacs.d/savefile/custom.el")
(load custom-file)

;; Auto-saves -- stop sticking foo~ files everywhere, jfc
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/autosave"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Don't make me type "yes" all the time
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package dired+
  :init (setq diredp-hide-details-initially-flag nil))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package bookmark+
  :config
  (setq bookmark-default-file "~/Dropbox/emacs/gifs.bmk"
	bmkp-last-as-first-bookmark-file nil))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-completing-prompt)))

(use-package wgrep
  :demand t)

(use-package wgrep-helm
  :demand t)

(use-package wgrep-ag
  :demand t
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (add-hook 'helm-ag-mode-hook 'wgrep-ag-setup))

(provide 'orary-core)
;;; orary-core.el ends here
