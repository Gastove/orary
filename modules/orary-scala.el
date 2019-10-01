;;; orary-scala.el --- Scala configuration for Orary

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
;; Who doesn't like scala? Good news: this one is dead simple. Thx, ensime.
;;; Code:

;; (defun orary/make-fantomas-tmp-buffer ()
;;   (-let [buf (get-buffer-create "*Fantomas Output*")]
;;     (with-current-buffer buf
;;       (fsharp-mode)
;;       (read-only-mode))
;;     buf))

(defun orary/scala-format-buffer (arg)
  "Formats the current buffer with `fantomas' if installed; see
  https://github.com/fsprojects/fantomas for details. With prefix
  argument ARG, shows a preview of the output instead of
  overwriting the current buffer."
  (interactive "P")
  (-if-let (scalariform-command (executable-find "scalariform"))
      (-let [target-buffer (if arg
                               (orary/make-fantomas-tmp-buffer)
                             (buffer-name))]
        (shell-command-on-region
         (point-min)
         (point-max)
         (s-join " "
                 (list
                  scalariform-command
                  "--stdin --stdout"
                  ))
         target-buffer))
    (message "Couldn't find `scalariform' on your path"))
  )

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (add-hook
   'scala-mode-hook
   (lambda ()
     (subword-mode +1)
     (lsp))))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; (use-package ensime
;;   :ensure t
;;   :config
;;   (setq ensime-startup-notification nil)
;;   )

(provide 'orary-scala)
;;; orary-scala.el ends here
