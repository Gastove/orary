;;; orary-go.el --- Go Programming in Orary
;;
;; Copyright (C) 2016 Ross Donaldson

;; Author: Ross Donaldson <gastove@gmail.com>
;; URL: https://github.com/Gastove/

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

;;
;;; Commentary:
;;
;;; Code:

(require 'f)
(require 'dash)

(use-package company-go)
(-let* ((go-path (getenv "GOPATH"))
        (oracle-path (f-expand "src/golang.org/x/tools/cmd/oracle/oracle.el" go-path)))
  (if (f-exists? oracle-path)
      (load oracle-path)))

(use-package go-mode
  :config
  (defun go-mode-config ()
    (add-to-list 'company-backends 'company-go))

  (add-hook 'go-mode-hook 'go-mode-config))

(provide 'orary-go)
;;; orary-go.el ends here
