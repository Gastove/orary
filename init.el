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

;; We're about to use this
(setq custom-file "~/.emacs.d/savefile/custom.el")

;; Make sure the package index has been loaded at least once before we try to
;; do any installing. After, make a note that we don't have to do this again.
(unless (and (boundp 'orary/packages-loaded-atleast-once)
             orary/packages-loaded-atleast-once)
  (progn
    (package-refresh-contents)
    (customize-save-variable 'orary/packages-loaded-atleast-once t)))

;; You know what I hate? Weird stale org installs. Can we not?
(unless (and (boundp 'orary/fresh-org-installed)
             orary/fresh-org-installed)
  (progn
    (package-install 'org)
    (customize-save-variable 'orary/fresh-org-installed t)))

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
  "Install PACKAGE."
  (unless (package-installed-p package)
    (package-install package)))

(defun orary/install-packages ()
  "Install all the packages in orary/packages."
  (mapc #'orary/install-package orary/packages))

(orary/install-packages)

;; Gets us string-join
(require 'subr-x)

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
