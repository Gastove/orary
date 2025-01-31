;;; orary-projectile.el --- Projectile errwhrr
;;
;;; Commentary:
;; YAS
;;; Code:

;;(setq projectile-keymap-prefix (kbd "C-c p"))

(require 'orary-helm)
(require 'magit)
(require 'f)

(use-package perspective
  :straight t
  :config
  (setq persp-show-modestring nil)
  :custom
  (persp-mode-prefix-key (kbd "C-x m p"))
  :bind (:map perspective-map
              ("p" . projectile-persp-switch-project)))

(use-package persp-projectile)
(use-package ag)

(defun orary/projectile-switch-project-action ()
  (let ((proj-name (projectile-project-name))
        (proj-root (projectile-project-root))
        (windows (orary/three-windows)))

    (select-window (plist-get windows :left-window))
    (switch-to-buffer (format "*scratch* (%s)" proj-name))
    (select-window (plist-get windows :right-window))
    (if (magit-git-repo-p proj-root)
        (magit-status proj-root)
      (dired proj-root))
    (select-window (plist-get windows :middle-window))
    (-let ((fully-qualified-md (f-expand "README.md" proj-root))
           (fully-qualified-org (f-expand "README.org" proj-root))
           (blog-file (f-expand "blog.blorg" proj-root))
           (the-daily (f-expand "cotidienne.org" proj-root)))
      (cond
       ((f-exists? fully-qualified-md) (find-file fully-qualified-md))
       ((f-exists? fully-qualified-org) (find-file fully-qualified-org))
       ((f-exists? blog-file) (find-file blog-file))
       ((f-exists? the-daily) (find-file the-daily))
       (:else (dired proj-root))))
    ))


(use-package helm-projectile
  :after helm
  :straight t)

(defvar orary/projectile-ignore-prefixes
  '("~/.cargo/registry" "~/.rustup/toolchains" "/nix"))

(defun orary/ignore-project? (truename)
  (-any? (lambda (maybe-parent)
           (f-ancestor-of? maybe-parent truename)) orary/projectile-ignore-prefixes))

;; NOTE[gastove|2022-12-24] A handy little utility doodah for cleaning up the project list.
(defun orary/clean-project-list ()
  (interactive)
  (-map
   (lambda (project)
     (when (orary/ignore-project? project)
       (projectile-remove-known-project project)))
   projectile-known-projects))

(use-package projectile
  :straight t
  :after helm
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
        projectile-create-missing-test-files t
        projectile-sort-order 'recent-active
        projectile-indexing-method 'alien
        projectile-ignored-project-function #'orary/ignore-project?)
  (projectile-update-project-type
   'dotnet-sln
   :test-dir #'orary/find-fsharp-test-dir
   :test-suffix "Tests")

  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("p" . projectile-persp-switch-project)
              ("s s" . orary/helm-do-rg-in-project)))

(provide 'orary-projectile)
;;; orary-projectile.el ends here
