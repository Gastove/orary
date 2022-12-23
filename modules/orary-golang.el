;;; orary-golang.el --- Golang Programming in Orary
;;
;; Copyright (C) 2016 Ross Donaldson

;; Author: Ross Donaldson <gastove@gmail.com>
;; URL: https://github.com/Gastove/

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

;;
;;; Commentary:
;;
;;; Code:

(require 'f)
(require 'dash)
(require 'orary-braces)
(require 'projectile)

(defun orary/go-ret-dwim (arg)
  (interactive "P")

  (-let [begin-curr-line (line-beginning-position)]
    (unless (looking-at "}")
      (sp-insert-pair "{"))
    (when arg
      (end-of-line)
      (insert ",")
      (backward-char 2))
    (orary/braces-open-pair)
    (if (and arg
             (save-excursion
               (re-search-backward "\\[\\]\\(.*\\)\\>" begin-curr-line t)))
        (progn
          (insert (match-string 1))
          (sp-insert-pair "{")
          (end-of-line)
          (insert ",")
          (backward-char 2)
          (orary/braces-open-pair))
      )))

(defun orary/go-packages-go-list-restricted ()
  "A modified package listing function which lists only:
   1. Standard lib
   2. Packages defined in a given project
   3. Packages vendored inside the current project. "
  (-let* ((proj (projectile-project-root))
          (gopath (getenv "GOPATH"))
          (go-pkg-root (s-chop-prefix (concat gopath "/src/") proj))
          (pkg-vendor-dir (concat go-pkg-root "vendor/"))
          (std (process-lines "go" "list" "-e" "std"))
          (pkg (process-lines "go" "list" "-e" (concat go-pkg-root "...")))
          (vendor (-map
                   (lambda (line) (s-chop-prefix pkg-vendor-dir line))
                   (process-lines "go" "list" "-e" (concat pkg-vendor-dir "...")))))

    (-concat std pkg vendor)))

;; NOTE[rdonaldson|2021-03-31] This is a good idea for Eventually; right now it
;; doesn't appear to work properly, so we'll use checker-chaining instead.
;; (defvar orary/lsp-go-enable-staticcheck t)
(lsp-register-custom-settings
 '(("gopls.staticcheck" t t)))

(use-package go-mode
  :config
  (setq go-packages-function #'orary/go-packages-go-list-restricted
        gofmt-command "goimports"
        flycheck-go-build-install-deps t
        lsp-go-use-placeholders t
        )

  (defun go-mode-config ()
    (setq-local comment-start "//")
    (subword-mode +1)
    (setq indent-tabs-mode t)
    (lsp)    

    ;; go-fmt on save.
    ;; This is... in many ways a good idea, but also *so irritating* that
    ;; it's hard to deal with. (Getting gofmt error buffers any time you wanna
    ;; save and do something else -- say, change buffers -- is rough.)
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (setq-local company-dabbrev-downcase nil)
    )

  (add-hook 'go-mode-hook 'go-mode-config)
  ;; (add-hook 'go-mode-hook #'go-eldoc-setup)
  :bind (:map go-mode-map
              ("C-c n" . gofmt)
              ("C-<return>" . orary/go-ret-dwim)
              ("C-o" . orary/braces-open-newline))
  )

(provide 'orary-golang)
;;; orary-go.el ends here
