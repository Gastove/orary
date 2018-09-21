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
(use-package unicode-fonts
  :demand t
  :config (unicode-fonts-setup))

(use-package all-the-icons
  :ensure t
  :config
  ;; Install fonts explicitly if they haven't been already on Linux
  (if (and
       (eq system-type 'gnu/linux)
       (not (f-exists? (f-expand ".local/share/fonts/all-the-icons.ttf" orary/user-home-dir))))
      (all-the-icons-install-fonts)))

;; Neotree
(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)        
        neo-window-fixed-size nil))

(use-package smart-mode-line
  :config (sml/setup)
  (setq rm-blacklist '(" MRev"))

  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)

  ;; Java and scala package names are infinite and terrible; shorten them.
  (add-to-list 'sml/replacer-regexp-list '("^~/Code/" ":C:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:C:\\(?:.*\\)\\{1,2\\}/src/main/java/" ":SMJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:C:\\(?:.*\\)\\{1,2\\}/src/test/java/" ":STJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:C:\\(?:.*\\)\\{1,2\\}/src/main/scala/" ":SMS:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:C:\\(?:.*\\)\\{1,2\\}/src/test/scala/" ":STS:") t)

  ;; Make sure I notice when I'm in
  (add-to-list 'rm-text-properties '(" Sp/s" 'face 'font-lock-warning-face)))

(use-package beacon
  :demand t
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-focused t))

(provide 'orary-ui)
;;; orary-ui.el ends here
