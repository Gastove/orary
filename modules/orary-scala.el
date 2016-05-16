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

(use-package ensime
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (add-hook 'scala-mode-hook (lambda ()
                               (setq tab-width 2)
                               (subword-mode +1))))

(provide 'orary-scala)
;;; orary-scala.el ends here
