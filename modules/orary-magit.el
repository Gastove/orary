;;; orary-magit.el --- Who doesn't love Magit?
;;
;;; Commentary:
;;
;;; Code:


;; Duo two-factor login for rdonaldson

;; Enter a passcode or select one of the following options:

;;  1. Duo Push to XXX-XXX-4294
;;  2. Phone call to XXX-XXX-4294
;;  3. SMS passcodes to XXX-XXX-4294

(unbind-key "s-m")
(use-package magit
  :pin melpa-stable
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"
        magit-branch-read-upstream-first t
        magit-branch-arguments nil
        magit-push-arguments '("--set-upstream")
        magit-push-always-verify nil
        magit-revert-buffers t
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; (add-to--list 'magit-process-password-prompt-regexps )
  :bind (("s-m m" . magit-status)
         ("C-x g" . magit-status)
         ("s-m l" . magit-log)
         ("s-m b" . magit-blame)))

(use-package git-timemachine
  :bind ("s-m t" . git-timemachine))

(provide 'orary-magit)
;;; orary-magit.el ends here
