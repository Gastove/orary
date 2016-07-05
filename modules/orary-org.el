;;; orary-org.el --- Configuration for org-mode in Orary
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
;;  Make Org Go!
;;; Code:

(require 'f)

;; Org
(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

;; Nice unicode bullet symbols for org
(use-package org-bullets)
;; All kinds of add-ons and extensions to org, can't even remember what all is in there :P
(use-package org-plus-contrib)
;; Additional exporters
(use-package ox-gfm)
(use-package ox-rst)

(use-package org
  :config
  (add-hook 'org-mode-hook (lambda ()
                             ;; Make sure auto-fill-mode is on. Pretty much always need it.
                             (turn-on-auto-fill)
                                        ;
                             ;; Dramatically improve company completion in org Org uses the `pcomplete'
                             ;; system; wire it up
                             (add-pcomplete-to-capf)
                             (require 'org-bullets)
                             (org-bullets-mode 1)))
  (require 'ox-confluence)
  ;; TODO Keyword states:
  ;; > In-Progress states: BACKLOG, TODO, DOING, BLOCKED
  ;; > Finished states:    DONE, IMPOSSIBLE, CANCELLED
  (setq org-todo-keywords
        '((sequence "BACKLOG(k)")
          (sequence "TODO(t)" "DOING(o)" "|" "DONE(d)")
          (sequence "BLOCKED(b)" "|" "UNBLOCKED (u)" "CANCELLED(c)" "IMPOSSIBLE(i)"))

        org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("DOING" . org-todo)
          ("BACKLOG" . org-todo)
          ("BLOCKED" . org-warning)
          ("CANCELLED" . org-done)
          ("IMPOSSIBLE" . org-done)
          ("DONE" . org-done))

        ;; Config org export backends
        org-export-backends `(ascii
                              beamer
                              confluence
                              deck
                              gfm
                              gnuplot
                              html
                              md
                              rst)

        ;; Export defaults: no table of contents, no numbered headers, don't convert ^
        ;; or _ to superscripts
        org-export-with-section-numbers nil
        org-export-with-sub-superscripts nil
        org-export-with-toc nil

        ;; Don't ask before src block eval
        org-confirm-babel-evaluate nil

        ;; Refiling defaults
        org-refile-targets '((org-agenda-files . (:maxlevel . 5)))
        org-refile-allow-creating-parent-nodes 'confirm

        org-agenda-text-search-extra-files '(agenda-archives)

        ;; Hide org emphasis marks
        org-hide-emphasis-markers t

        ;; Start indented
        org-startup-indented t

        ;; Stop folding. Just... stop.
        org-startup-folded t

        ;; Fontify inside code blocks
        org-src-fontify-natively t

        org-default-notes-file (f-expand"~/Dropbox/org-docs/cotidienne.org")

        org-capture-templates
        `(("t" "Todo" entry (file+headline "" "General To-Dos")
           "** TODO %?\n")
          ("f" "File-Todo" entry (file+headline "" "General To-Dos")
           "** TODO %?\n %i\n %A\n")
          ("e" "Email" entry (file+headline "" "General To-Dos")
           ,(string-join '("** TODO Reply to %:from re:%?"
                           "DEADLINE: <%(org-read-date nil nil \"+1d\")>"
                           "\%i"
                           "%a\n")
                         "\n"))
          ("g" "Log" entry (file+headline "" "Log") "** email%?\n %l")))

  ;; Jump and Sparse-Tree contexts
  (push  '(org-goto . local) org-show-context-detail)
  (push '(tags-tree . local) org-show-context-detail)

  ;; Support for Babel Mode code blocks
  ;; NOTE: requires the addition of the org elpa repo!
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (emacs-lisp . t)
                                 (java . t)
                                 (sh . t)
                                 (R . t)
                                 (scala . t)
                                 (scheme . t)
                                 (sql . t)))

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)))

(provide 'orary-org)
;;; orary-org.el ends here
