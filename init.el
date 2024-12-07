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
;; NOTE[Donaldson|2022-12-12] As of now, it looks like I get to convert to
;; straight.el. I don't actually want to do this, so that's fun, but it _does_
;; seem to be good stuff. Unfortunately, installing Helm from melpa is
;; exploding, so. Cool.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; (require 'package)
;; (package-initialize)
;; (setq package-check-signature nil)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; We use use-package absolutely everywhere. Make sure it's there.
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; Sometimes you want emacs to load faster. This is for those times.
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(eval-when-compile
  (require 'use-package))
(use-package diminish)             ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Package Loading and Configuration
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; New Core
(use-package dash                      ;; Modern FP, combiners
  :straight t
  :config
  (dash-enable-font-lock))
(use-package s                         ;; String manipulation
  :straight t
  )
(use-package f                         ;; File manipulation
  :straight t)

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
  :ensure t)

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
(require 'orary-helm)
(require 'orary-projectile)
(require 'orary-emote)
(require 'orary-smartparens)
(require 'orary-lang-server)
(require 'orary-tree)

;; Talky -- communicate with others
(require 'orary-irc)

;; Languages
(require 'orary-clojure)
(require 'orary-elixir)
(require 'orary-emacs-lisp)
(require 'orary-fsharp)
(require 'orary-csharp)
(require 'orary-formats)
(require 'orary-golang)
(require 'orary-java)
(require 'orary-javascript)
(require 'orary-kotlin)
(require 'orary-lilypond)
(require 'orary-ocaml)
(require 'orary-org)
(require 'orary-purescript)
(require 'orary-python)
(require 'orary-rust)
(require 'orary-scala)
;; Possibly ESS is fucking with emacsclient and the server? Unclear. Investigate.
;; (require 'orary-stats)
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
