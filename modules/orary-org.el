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
(require 's)

;; Org
(defun add-pcomplete-to-capf ()
  "Loads pcomplete functions in to completion-at-point-functions
so company-mode will work nicely."
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

;; Nice unicode bullet symbols for org
(use-package org-bullets)
;; All kinds of add-ons and extensions to org, can't even remember what all is in there :P
(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

;; Additional exporters
(use-package ox-gfm)
(use-package ox-rst)
(use-package ox-pandoc)
;; Additional org-babel bindings
(use-package ob-restclient)
(require 'ob-clojure)
(require 'cider) ;; for ob-clojure config
(use-package ob-async)

(use-package org
  :ensure org-plus-contrib
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
                              pandoc
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

        ;; Let me refile via the full outline path of a header
        org-refile-use-outline-path 't
        org-outline-path-complete-in-steps nil

        org-agenda-text-search-extra-files '(agenda-archives)

        ;; Hide org emphasis marks
        org-hide-emphasis-markers t

        ;; Start indented
        org-startup-indented t

        ;; Stop folding. Just... stop.
        org-startup-folded t

        ;; Fontify inside code blocks
        org-src-fontify-natively t

        org-default-notes-file (f-expand "~/Dropbox/org-docs/cotidienne.org")

        org-capture-templates
        `(("t" "Todo" entry (file+headline "" "General To-Dos")
           "** TODO %?\n")
          ("w" "Work Todo" entry (file+headline ,(f-expand "~/Documents/work.org") "General To-Dos")
           "** TODO %?\nDEADLINE: <%(org-read-date nil nil \"+1d\")>\n%^{trello_ticket}p" :empty-lines 1)
          ("n" "Work Notes" entry (file+headline ,(f-expand "~/Documents/work.org") "Captured Notes")
           "** %T %^{PROMPT}\n%?")
          ("f" "File-Todo" entry (file+headline "" "General To-Dos")
           "** TODO %?\n %i\n %A\n")
          ("e" "Email" entry (file+headline "" "General To-Dos")
           ,(s-join "\n" '("** TODO Reply to %:from re:%?"
                           "DEADLINE: <%(org-read-date nil nil \"+1d\")>"
                           "\%i"
                           "%a\n")))
          ("g" "Work Log" entry (file+headline "" "Log")
           "** %T\n:PROPERTY:\n:capture_location: %a\n:END:\n%?""** %T\n%?")
          ("l" "Personal Log" entry (file+headline "" "Log")
           "** %T\n:PROPERTY:\n:capture_location: %a\n:END:\n%?"))

        ;; The Agenda
        ;; Show me a 10 day view by default
        org-agenda-span 10)
  (add-hook 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)

  ;; Structural templates
  (add-to-list 'org-structure-template-alist
               '(("ep" "#+BEGIN_EXAMPLE python\n?\n#+END_EXAMPLE")
                 ("sp" "#+BEGIN_SRC python\n?\n#+END_SRC")
                 ("ss" "#+BEGIN_SRC sh\n?\n#+END_SRC")))

  ;; Jump and Sparse-Tree contexts
  (push  '(org-goto . local) org-show-context-detail)
  (push '(tags-tree . local) org-show-context-detail)

  ;; Support for Babel Mode code blocks
  ;; NOTE: requires the addition of the org elpa repo!
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure .t)
                                 (emacs-lisp . t)
                                 (java . t)
                                 (restclient . t)
                                 (R . t)
                                 (scala . t)
                                 (scheme . t)
                                 (shell . t)
                                 (sql . t)
                                 (python . t)))

  (setq org-babel-clojure-backend 'cider)

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)))

(provide 'orary-org)
;;; orary-org.el ends here
