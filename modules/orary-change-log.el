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

;;; Code:

(require 'yasnippet)
(require 'dash)
(require 'f)


(defvar orary/root-dirs
  '(".git" ".hg" ".bzr" ".projectile"))

(defvar orary/change-log-name "CHANGELOG.md")

(defvar orary/change-log-entry-snippet
  "## [`clp-proj-version`] - `(orary/insert-iso-date)`\n$0"
  "A yasnippet snippet for adding a new change log entry.")

(defun orary/project-root-p (dir)
  (-filter (lambda (d) (f-exists? (f-expand d dir))) orary/root-dirs))

(defun orary/clp-find-project-root ()
  (interactive)
  (f-traverse-upwards 'orary/project-root-p default-directory))

(defun orary/create-new-change-log (log-name)
  (create-file-buffer log-name))

(defun orary/find-or-create-change-log ()
  (interactive)
  (-let* ((project-root (orary/clp-find-project-root))
          (log-name (f-expand orary/change-log-name project-root))
          (change-log-buf (find-file log-name)))
    (set-buffer change-log-buf)
    (switch-to-buffer change-log-buf)))

(defun orary/insert-change-log-heading ()
  (interactive)
  (yas-expand-snippet orary/change-log-entry-snippet
                      (point) (point)
                      '((clp-proj-version "0.1.0"))))

(defun orary/insert-change-log-entry ()
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
