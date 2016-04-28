;;; orary-ui.el --- Core UI configuration for orary
;;
;;; Commentary:
;;
;;; Code:

;; Appearance
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'rhombus t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(which-function-mode -1)
(setq initial-buffer-choice t)
(setq column-number-mode t)

;; Alerts
(defun orary/visible-bell ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function #'ignore)
(setq visible-bell nil ring-bell-function 'orary/visible-bell)

(use-package unicode-fonts
  :demand t
  :config (unicode-fonts-setup))

(use-package smart-mode-line
  :config (sml/setup)
  (setq rm-blacklist '(" MRev"
		       " yas"
		       " Helm"
		       " company"
		       " guru"
		       " Pre"
		       " Rbow"))

  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)

  ;; Java and scala package names are infinite and terrible; shorten them.
  (add-to-list 'sml/replacer-regexp-list '("^~/Code/" ":CODE:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/main/java/" ":SMJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/test/java/" ":STJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/main/scala/" ":SMS:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/test/scala/" ":STS:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:SM[JS]:com/urbanairship/\\(.*\\)/" ":M:\\1:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:ST[JS]:com/urbanairship/\\(.*\\)/" ":T:\\1:") t)

  ;; Make sure I notice when I'm in
  (add-to-list 'rm-text-properties '(" Sp/s" 'face 'font-lock-warning-face)))

(provide 'orary-ui)
;;; orary-ui.el ends here
