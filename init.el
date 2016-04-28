;;; init.el --- Init file
;;
;;; Commentary:
;;  Yep.  We got code.
;;; Code:

;; TODO: Make a unified var for saving info like place, history.
;; TODO: move multi-account Stuffz to work congifs so they don't clutter this up
;; TODO: Magit colors are all fucked, but only sometimes
;; TODO: why don't I have persistent helm?
;; TODO: my version of C-c n isn't doing what I expected; fix this.
;; TODO: flyspell
;; TODO: load todos in project
;; TODO: ace-window
;; TODO: ace jump to char
;; TODO: Emooootes!
;; TODO: Find a way to make package load order not matter

;;; Package Management

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

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
