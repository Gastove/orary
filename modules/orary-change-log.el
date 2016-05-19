;;; orary-change-log.el --- Change Log Editing in Orary
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
;; Do you keep a change log? You should. http://keepachangelog.com/ Note that
;; this isn't actually a change log "mode"; in fact, it explicitly disables the
;; existing GNU change log mode. Instead, `change-log-plus' takes the approach
;; that you'll want the major mode of your change log (probably markdown, but
;; maybe org or rst) to do the mode-heavy-lifting, and instead tries to provide
;; sensible global functions for easily adding to your log.
;;
;; We'll differentiate between an `entry', e.g.:
;;
;; ## [0.1.0] - 2016-05-02
;;
;; A `heading', .e.g.:
;;
;; ### General
;;
;; And an item:
;;
;; - Substantial refactor for code clarity

;;; Code:

(require 'yasnippet)
(require 'dash)
(require 'f)


(defvar orary/root-dirs
  '(".git" ".hg" ".bzr" ".projectile")
  "What do we recognize as a `project'?")

(defvar orary/change-log-name
  "CHANGELOG.md"
  "What is your changelog called?")

(defvar orary/project-version nil
  "What's the version of this project?")

(make-local-variable orary/project-version)

;;-----------------------------------Markers-------------------------------------
(defvar orary/change-log-entry-marker "##")

(defvar orary/change-log-heading-marker "###")

;;----------------------------------Snippets------------------------------------
(defvar orary/change-log-entry-snippet
  (format "%s [`clp-proj-version`] - `(orary/insert-iso-date)`\n$0"
          orary/change-log-entry-marker)
  "A yasnippet snippet for adding a new change log entry.")

(defvar orary/change-log-heading-snippet
  (format "%s ${1:`clp-header`}\n$0"
          orary/change-log-heading-marker))

;;-----------------------------------regexen-------------------------------------


(defvar orary/change-log-header-regexp
  "\\[0\.1\.0\\]"
  (let ((version-string (regexp-quote orary/project-version)))))


(defun orary/project-root-p (dir)
  (-filter (lambda (d) (f-exists? (f-expand d dir))) orary/root-dirs))

(defun orary/clp-find-project-root ()
  (interactive)
  (f-traverse-upwards 'orary/project-root-p default-directory))

(defun orary/create-new-change-log (log-name)
  (create-file-buffer log-name))

;; TODO: the `find' part of this should probably consider other
;; spelling/capitalization variants -- ChangeLog, CHANGE-LOG -- and different
;; extensions -- md, org, rst, txt at least.
(defun orary/find-or-create-change-log ()
  (interactive)
  (-let* ((project-root (orary/clp-find-project-root))
          (log-name (f-expand orary/change-log-name project-root))
          (change-log-buf (find-file log-name)))
    (set-buffer change-log-buf)
    (switch-to-buffer change-log-buf)))

(defun orary/change-log-insert (thing)
  (interactive)
  (-let (()))
  (cond
   ((eq thing 'entry) (yas-expand-snippet orary/change-log-entry-snippet
                                          (point) (point)
                                          '((clp-proj-version "0.1.0"))))))

(defun orary/insert-change-log-entry ()
  (interactive)
  (goto-char 1)
  (open-line 1)
  (yas-expand-snippet orary/change-log-entry-snippet
                      (point) (point)
                      '((clp-proj-version "0.1.0"))))

(defun orary/insert-change-log-heading ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "### [0-9]\\.[0-9]\\.[0-9]")
    (forward-char 1)
    (insert "- hey hey hey")
    (insert "\n")))



(defvar orary/change-log-prefix)
(define-prefix-command orary/change-log-prefix)

;; Commands, somehow:
;; Goto-or-create-change-log
;; Insert new header
;; Insert new subheadings: General, Addition, Deprecation, Breaking Change
;; Insert new item under subheading

(defun orary/change-log-plus-setup ()
  (rassq-delete-all 'change-log-mode auto-mode-alist))

(provide 'orary-change-log)
;;; orary-change-log.el ends here
