;;; init.el --- Init file
;;
;;; Commentary:
;;  Yep.  We got code.
;;; Code:

;;-----------------------------Package Management-------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; Make sure the package index has been loaded at least once before we try to
;; do any installing. After, make a note that we don't have to do this again.
(when (not package-archive-contents)
  (package-refresh-contents))

;; We use use-package absolutely everywhere. Make sure it's there.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Package Loading and Configuration
(setq use-package-always-ensure t)

;; New Core
(use-package dash)                      ;; Modern FP, combiners
(use-package s)                         ;; String manipulation
(use-package f)                         ;; File manipulation

;; We're about to use this
(defvar orary/orary-root-dir (file-name-directory
                              (or (buffer-file-name) (file-chase-links load-file-name))))

(add-to-list 'load-path (f-expand "modules/" orary/orary-root-dir))

;; Load this first in case things need executables from the path.
(require 'orary-osx)


(require 'orary-core)
(require 'orary-ui)

(require 'orary-editor)
(require 'orary-programming)
(require 'orary-text)

(require 'orary-company)
(require 'orary-emote)
(require 'orary-functions)
(require 'orary-helm)
(require 'orary-jabber)
(require 'orary-keymap)
(require 'orary-magit)
(require 'orary-mu4e)
(require 'orary-org)
(require 'orary-projectile)
(require 'orary-smartparens)
(require 'orary-work)

(require 'orary-emacs-lisp)
(require 'orary-nxml)
(require 'orary-purescript)
(require 'orary-python)
(require 'orary-scala)

;; Last but not least, click on the server
(server-start)

(provide 'init)

;;; init.el ends here
