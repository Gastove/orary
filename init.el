;;; init.el --- Init file
;;
;;; Commentary:
;;  Yep.  We got code.
;;; Code:

;;; Package Management

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; Make sure the package index has been loaded at least once before we try to
;; do any installing. After, make a note that we don't have to do this again.
(unless (bound-and-true-p 'orary/packages-loaded-atleast-once)
  (progn
    (package-refresh-contents)
    (customize-save-variable 'orary/packages-loaded-atleast-once t)))

;; Packages it just doesn't make sense to manage with use-package
(defvar orary/packages '(dash
			 exec-path-from-shell
			 helm-ag
			 helm-projectile
			 org-plus-contrib
			 org-bullets
			 rainbow-mode
			 rainbow-delimiters
			 use-package))

(defun orary/install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun orary/install-packages ()
  (mapc #'orary/install-package orary/packages))

(orary/install-packages)

;;; Utilities we want globally
(require 'dash)   ;; Way better list manipulation functions
(require 'subr-x) ;; Gets us string-join


(defvar orary-dir (file-name-directory
		   (or (buffer-file-name) (file-chase-links load-file-name))))

(add-to-list 'load-path (expand-file-name "modules/" orary-dir))

;; Package Loading and Configuration
(setq use-package-always-ensure t)

;; Load this first in case things need executables from the path.
(require 'orary-osx)

(require 'orary-company)
(require 'orary-core)
(require 'orary-ui)

(require 'orary-programming)
(require 'orary-lisp)

(require 'orary-helm)
(require 'orary-magit)
(require 'orary-org)

(require 'orary-emote)
(require 'orary-jabber)
(require 'orary-projectile)
(require 'orary-smartparens)
(require 'orary-mu4e)
(require 'orary-emacs-lisp)
(require 'orary-python)
(require 'orary-scala)
(require 'orary-purescript)
(require 'orary-nxml)
(require 'orary-functions)
(require 'orary-keymap)
(require 'orary-work)

;; Last but not least, click on the server
(server-start)

(provide 'init)

;;; init.el ends here
