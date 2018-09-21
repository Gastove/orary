;;; orary-projectile.el --- Projectile errwhrr
;;
;;; Commentary:
;; YAS
;;; Code:

(setq projectile-keymap-prefix (kbd "C-c p"))

(require 'orary-helm)

(use-package persp-projectile)
(use-package ag)

(defun orary/projectile-switch-project-action ()
  (let ((proj-name (projectile-project-name))
        (proj-root (projectile-project-root))
        (windows (orary/three-windows)))
    (message "Trying to switch to project %s with root %s" proj-name proj-root)
    (select-window (plist-get windows :left-window))
    (switch-to-buffer (format "*scratch* (%s)" proj-name))
    (select-window (plist-get windows :right-window))
    (magit-status-internal proj-root)
    (select-window (plist-get windows :middle-window))
    (-let ((fully-qualified-md (f-expand "README.md" proj-root))
           (fully-qualified-org (f-expand "README.org" proj-root)))
      (cond
       ((f-exists? fully-qualified-md) (find-file fully-qualified-md))
       ((f-exists? fully-qualified-org) (find-file fully-qualified-org))
       (:else (dired proj-root))))
    ))

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
        projectile-switch-project-action #'orary/projectile-switch-project-action        
        projectile-create-missing-test-files t)
  ;; :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-persp-switch-project))
  )

(provide 'orary-projectile)
;;; orary-projectile.el ends here
