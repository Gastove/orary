;;; orary-projectile.el --- Projectile errwhrr
;;
;;; Commentary:
;; YAS
;;; Code:

(require 'orary-helm)

(use-package persp-projectile)
(use-package ag)
(use-package projectile
  :demand t
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
        projectile-switch-project-action 'projectile-dired)
  :bind-keymap ("s-p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-persp-switch-project)))

(provide 'orary-projectile)
;;; orary-projectile.el ends here
