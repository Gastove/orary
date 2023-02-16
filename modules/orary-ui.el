;;; orary-ui.el --- Core UI configuration for orary
;;
;;; Commentary:
;;
;;; Code:

(use-package diminish)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'rhombus t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(which-function-mode -1)
(setq initial-buffer-choice t)
(setq column-number-mode t)

(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(fullscreen . fullscreen)))

;; Alerts
(defun orary/visible-bell ()
  "Replace the bell sound with a visible bell."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function #'ignore)
(setq visible-bell nil ring-bell-function 'orary/visible-bell)

;; Fonts
(use-package persistent-soft)
(use-package unicode-fonts
  :demand t
  :config (unicode-fonts-setup))

(use-package all-the-icons  
  :config
  ;; Install fonts explicitly if they haven't been already on Linux
  (if (and
       (eq system-type 'gnu/linux)
       (not (f-exists? (f-expand ".local/share/fonts/all-the-icons.ttf" orary/user-home-dir))))
      (all-the-icons-install-fonts)))


(eval
 (rhombus/with-color-variables
   `(defun orary/set-cursor-by-state ()
      "Changes cursor color depending on attributes of the current buffer."
      (cond
       (buffer-read-only
        (set-cursor-color ,rhombus-warn))

       ((crux-already-root-p)
        (set-cursor-color ,rhombus-teal))

       (t
        (set-cursor-color ,rhombus-grey))))))

(add-hook 'post-command-hook #'orary/set-cursor-by-state)

(setq inhibit-compacting-font-caches t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'projectile))

(use-package beacon
  :demand t
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-focused t))

(provide 'orary-ui)
;;; orary-ui.el ends here
