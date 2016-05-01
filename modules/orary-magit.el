;;; orary-magit.el --- Who doesn't love Magit?
;;
;;; Commentary:
;;
;;; Code:

(unbind-key "s-m")
(use-package magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"
	magit-branch-read-upstream-first t
	magit-branch-arguments nil
	magit-push-arguments '("--set-upstream")
	magit-push-always-verify nil
	magit-revert-buffers t)
  :bind (("s-m m" . magit-status)
	 ("C-x g" . magit-status)
	 ("s-m l" . magit-log)
	 ("s-m b" . magit-blame)))

(use-package git-timemachine
  :bind ("s-m t" . git-timemachine))

(provide 'orary-magit)
;;; orary-magit.el ends here
