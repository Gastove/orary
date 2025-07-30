;;; orary-stats.el --- Statistical Programming in Orary -*- lexical-binding: t; -*-
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
;; Currently, this is mostly a configuration for ESS, which handles R and Julia
;; beautifully.
;;; Code:
(use-package ess
  :config
  (require 'ess-site)
  ;; (add-to-list 'ess-style-alist
  ;;              '(orary-ESS (ess-indent-offset . 2)
  ;;                          ;; (ess-offset-continued . 2)
  ;;                          ;; (ess-indent-from-lhs . 4)
  ;;                          ;; (ess-offset-arguments-newline . '(4))
  ;;                          ))

  ;; (ess-toggle-underscore nil)

  ;; (setq ess-default-style 'orary-ESS
  ;;       ;; ess-S-assign-key (kbd "M--")
  ;;       )

  (defun orary/inferior-R-configs ()
    (smartparens-mode 1))

  (defun orary/inferior-ess-configs ()
    (smartparens-mode 1))

  ;; (defun orary/ess-configs ()
  ;;   (ess-toggle-S-assign-key t))

  ;; Smartparens in the interpreter
  (add-hook 'ess-R-post-run-hook #'orary/inferior-R-configs)
  (add-hook 'inferior-ess-mode-hook #'orary/inferior-ess-configs)

  ;; Disable conversion of underscores to arrows; map to M-- instead
  (add-hook 'ess-mode-hook #'orary/ess-configs))

(provide 'orary-stats)
;;; orary-stats.el ends here
