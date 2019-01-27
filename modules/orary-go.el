;;; orary-go.el --- Go Programming in Orary
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

(use-package company-go
  :config
  (setq company-go-gocode-args '("-fallback-to-source")))

(use-package go-eldoc)
(use-package go-guru
  :demand t)


(flycheck-define-checker go-staticcheck
  "A Go checker that performs static analysis and linting using the `staticcheck'
command; formerly `megacheck'.

Explicitly compatible with \"the last two versions of go\"; can
target earlier versions (with limited features) if
`flycheck-go-version' is set. See URL `https://staticcheck.io/'."
  :command ("staticcheck"
            (option-list "-tags=" flycheck-go-build-tags concat)
            ;; Run in current directory to make megacheck aware of symbols
            ;; declared in other files.
            ".")
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes go-mode)


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

(use-package go-mode
  :config
  (setq go-packages-function #'orary/go-packages-go-list-restricted)
  (setq gofmt-command "goimports")
  (defun go-mode-config ()
    (setq-local comment-start "//")
    (subword-mode +1)
    (go-guru-hl-identifier-mode)
    ;; go-fmt on save.
    ;; This is... in many ways a good idea, but also *so irritating* that
    ;; it's hard to deal with. (Getting gofmt error buffers any time you wanna
    ;; save and do something else -- say, change buffers -- is rough.)
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (setq-local company-dabbrev-downcase nil)
    (add-to-list 'company-backends 'company-go)
    (add-to-list 'flycheck-checkers 'go-staticcheck)
    )

  (add-hook 'go-mode-hook 'go-mode-config)
  (add-hook 'go-mode-hook #'go-eldoc-setup)
  :bind (:map go-mode-map
              ("C-c n" . gofmt)
              ("C-<return>" . orary/go-ret-dwim)
              ("C-o" . orary/braces-open-newline))
  )

(provide 'orary-go)
;;; orary-go.el ends here
