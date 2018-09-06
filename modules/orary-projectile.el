;;; orary-projectile.el --- Projectile errwhrr
;;
;;; Commentary:
;; YAS
;;; Code:

(setq projectile-keymap-prefix (kbd "C-c p"))

(require 'orary-helm)

(use-package persp-projectile)
(use-package ag)

(use-package projectile
  :config
  (require 'persp-projectile)
  (require 'helm-projectile)
  (require 'ag)                         ; I sincerely cannot remember why this is here
  (require 'helm-ag)                    ; This either. Uh. Hrm.
  (persp-mode)
  (helm-projectile-on)
  (projectile-global-mode +1)
  (setq projectile-enable-caching t
        projectile-completion-system 'helm
        projectile-switch-project-action 'projectile-dired
        projectile-create-missing-test-files t)
  ;; :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-persp-switch-project))
  )

(provide 'orary-projectile)
;;; orary-projectile.el ends here
