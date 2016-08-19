;;; orary-javascript.el --- JavaScript configs for Orary
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
;; I don't JS a lot, but here's what I've got
;;; Code:

(use-package tern)
(use-package company-tern)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'company-dabbrev-code-modes 'js2-mode)
  (add-hook 'js2-mode-hook (lambda () (tern-mode +1) (subword-mode +1))))

(provide 'orary-javascript)
;;; orary-javascript.el ends here
