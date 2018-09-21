;;; orary-editor.el --- Configuring editor behaviors for src + txt
;;
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
;; Some behaviors should permeate `orary'. In general, this module brings in
;; tools that could apply to many languages (e.g. FlyCheck) or types of writing
;; (e.g. Org, Markdown), but per-language configuration is in the corresponding
;; orary module.

;;; Code:

(require 'dash)
(require 'f)

(global-prettify-symbols-mode 1)

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(delete-selection-mode 1)

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package easy-kill
  :demand t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package editorconfig
  :demand t)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))))

(use-package gist)

(require 're-builder)
(setq reb-re-syntax 'string)

;; This might be unneeded now? Could be nice. Let's test.
;; If not, yas-fallback-behavior is deprecated, so will have to find something else.
(defun yas-advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas-try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (unless (and (called-interactively-p 'interactive)
                        (yas-expand))
             ad-do-it))))

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-completing-prompt))
  (yas-advise-indent-function 'indent-for-tab-command)
  )

(use-package wgrep
  :demand t)

(use-package wgrep-helm
  :demand t)

(use-package wgrep-ag
  :demand t
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (add-hook 'helm-ag-mode-hook 'wgrep-ag-setup))

;;----------------------------------Flyspell------------------------------------
;; Spelling Checkingz
(require 'flyspell)
(require 'diminish)

(setq ispell-program-name "aspell"
      ;; The default mode is too slow, but `ultra' is too imprecise.
      ispell-extra-args '("--sug-mode=fast")
      ispell-personal-dictionary "~/Dropbox/emacs/.aspell.en.pws")

(defun orary/flyspell-text ()
  "Enable flyspell for text modes."
  (when (executable-find ispell-program-name)
    (flyspell-mode +1)
    (diminish 'flyspell-mode)
    (toggle-word-wrap +1)))

(defun orary/flyspell-programming ()
  "Enable flyspell for programming modes."
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode)
    (diminish 'flyspell-mode)))

(add-hook 'prog-mode-hook 'orary/flyspell-programming)
(add-hook 'text-mode-hook 'orary/flyspell-text)

(use-package rainbow-mode
  :diminish rainbow-mode)

;;; Undo
(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (-let [undo-tree-dir (f-expand "undo-tree" orary/save-root)]
    (unless (f-exists? undo-tree-dir)
      (f-mkdir undo-tree-dir))
    (setq undo-tree-history-directory-alist `((".*" . ,undo-tree-dir))
          undo-tree-auto-save-history t))
  :bind ("C-c u" . undo-tree-visualize))

;; Another fine trick from Bozhidar Batsov
(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

;; Always revert a buffer if the underlying file has changed on disk
(global-auto-revert-mode t)

;; make sure images revert too
(auto-image-file-mode +1)

(provide 'orary-editor)
;;; orary-editor.el ends here
