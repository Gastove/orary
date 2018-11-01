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

(use-package company-go)
(use-package go-eldoc)
(use-package go-guru
  :demand t)

(defun orary/go-open-braces (arg)
  (interactive "P")
  (unless (looking-at "}")
    (sp-insert-pair "{"))
  (when arg
    (end-of-line)
    (insert ",")
    (backward-char 2))
  (newline-and-indent)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun orary/go-packages-go-list-restricted ()
  "A modified package listing function which lists only:
   1. Standard lib
   2. Packages defined in a given project
   3. Packages vendored inside the current project. "
  (-let* ((proj (projectile-project-root))
          (gopath (getenv "GOPATH"))
          (go-pkg-root (s-chop-prefix (concat gopath "/src/") proj)))    
    (-concat
     (process-lines "go" "list" "-e" "std")
     (process-lines "go" "list" "-e" (concat go-pkg-root "..."))
     (process-lines "go" "list" "-e" (concat go-pkg-root "vendor/...")))))

(use-package go-mode
  :config  
  (setq go-packages-function #'orary/go-packages-go-list-restricted)
  (defun go-mode-config ()
    (setq-local comment-start "//")
    (subword-mode +1)
    (go-guru-hl-identifier-mode)
    ;; go-fmt This is... in many ways a good idea, but also *so irritating* that
    ;; it's hard to deal with. (Getting gofmt error buffers any time you wanna
    ;; save and do something else -- say, change buffers -- is rough.)
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    (add-to-list 'company-backends 'company-go))

  (add-hook 'go-mode-hook 'go-mode-config)
  (add-hook 'go-mode-hook #'go-eldoc-setup)
  :bind (:map go-mode-map
              ("C-c n" . gofmt)
              ("C-<return>" . orary/go-open-braces)
              ("C-c C-i" . company-complete))
  )

(provide 'orary-go)
;;; orary-go.el ends here
