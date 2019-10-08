;;; init.el --- Init file to bootstrap Orary!

;; Copyright (C) 2016 Ross Donaldson

;; Author: Ross Donaldson <gastove@gmail.com>
;; URL: https://github.com/Gastove/orary

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  Yep.  We got code.
;;; Code:

;;-----------------------------Package Management-------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Make sure the package index has been loaded at least once before we try to
;; do any installing. After, make a note that we don't have to do this again.
(unless package-archive-contents
  (package-initialize)
  (package-refresh-contents)
  )

;; We use use-package absolutely everywhere. Make sure it's there.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Package Loading and Configuration
(setq use-package-always-ensure t)

;; New Core
(use-package dash                      ;; Modern FP, combiners
  :config
  (dash-enable-font-lock))
(use-package s)                         ;; String manipulation
(use-package f)                         ;; File manipulation

;; A few vars of utility
(defvar orary/orary-root-dir (file-name-directory
                              (or (buffer-file-name) (file-chase-links load-file-name))))
(defvar orary/user-home-dir (getenv "HOME"))

;; Stuff I wrote
(add-to-list 'load-path (f-expand "modules/" orary/orary-root-dir))
;; Stuff from other people not available on MELPA
(add-to-list 'load-path (f-expand "vendor/" orary/orary-root-dir))

;; Load this first in case things need executables from the path.
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(if (eq system-type 'darwin)
    (require 'orary-osx)
  (require 'orary-linux))

;;------------------------------Let's Load Orary--------------------------------
;; Core -- central behaviors and configuration
(require 'orary-core)
(require 'orary-ui)
(require 'orary-functions)
(require 'orary-popups)
(require 'orary-cli)

;; Editing - global configs for editing in src and txt
(require 'orary-editor)
(require 'orary-keymap)
(require 'orary-programming)
(require 'orary-text)

;; Magit -- pretty much it's own thing. Double Mega.
(require 'orary-magit)

;; Minor Modes -- teamwork makes the dream work (●♡∀♡)
(require 'orary-company)
(require 'orary-projectile)
(require 'orary-helm)
(require 'orary-emote)
(require 'orary-smartparens)
(require 'orary-lang-server)
(require 'orary-tree)

;; Talky -- communicate with others
(require 'orary-irc)
(require 'orary-jabber)
;; Only load mu4e if it's there
;; (when (f-exists? "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
;;   (require 'orary-mu4e))
(require 'orary-notify)
(require 'orary-twitter)
(require 'orary-floobits)

;; Languages
(require 'orary-clojure)
(require 'orary-emacs-lisp)
(require 'orary-fsharp)
(require 'orary-formats)
(require 'orary-go)
(require 'orary-java)
(require 'orary-javascript)
(require 'orary-lilypond)
(require 'orary-ocaml)
(require 'orary-org)
(require 'orary-purescript)
(require 'orary-python)
(require 'orary-rust)
(require 'orary-scala)
(require 'orary-stats)
(require 'orary-web)
(require 'orary-misc)

;; Weird
(require 'orary-org-blorg)
;; Reading
(require 'orary-rss)

;; Twerk
(require 'orary-work)

;; Last but not least, click on the server
(server-start)

(provide 'init)

;;; init.el ends here
