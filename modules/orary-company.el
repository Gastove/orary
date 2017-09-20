;;; orary-company.el --- company-mode in Orary
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
;; Integrate company-mode in to orary for ALL the autocompletes.
;;; Code:

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)

  (setq company-idle-delay .4
        company-minimum-prefix-length 2
        company-tooltip-limit 20
        company-backends '(company-bbdb
                           company-nxml
                           company-css
                           company-eclim
                           company-semantic
                           company-clang
                           company-xcode
                           company-cmake
                           company-files
                           company-capf
                           (company-dabbrev-code company-gtags company-etags company-keywords)
                           company-oddmuse company-dabbrev))
  :diminish company)

(use-package company-tern)

(provide 'orary-company)
;;; orary-company.el ends here
