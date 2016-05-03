;;; orary-stats.el --- Statistical Programming in Orary
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
  (add-to-list 'ess-style-alist
               '(my-RRR (ess-indent-level . 2)
                        (ess-first-continued-statement-offset . 2)
                        ;; (ess-first-continued-statement-offset . 0)
                        (ess-continued-statement-offset . 0)
                        ;; (ess-continued-statement-offset . 4)
                        (ess-brace-offset . 0)
                        (ess-arg-function-offset . 4)
                        (ess-arg-function-offset-new-line . '(4))
                        (ess-expression-offset . 4)
                        (ess-else-offset . 0)
                        (ess-close-brace-offset . 0)))

  (setq ess-default-style 'my-RRR))

(provide 'orary-stats)
;;; orary-stats.el ends here