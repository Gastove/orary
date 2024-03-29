;;; orary-web.el --- Web-Mode integration for Orary
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
;; `web-mode' is the nicest way I've found to interact with HTML.
;;; Code:

(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.tmpl\\'"      . web-mode)
         ("\\.jsx\\'"       . web-mode)
         ("\\.tsx\\'"       . web-mode)
         ("\\.html\\'"      . web-mode)
         ("\\.hbs\\'"       . web-mode)
         ("\\.html\\.j2\\'"   . web-mode))
  :config
  (setq web-mode-enable-engine-detection t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-auto-close-style 2)
  (setq web-mode-engines-alist
        '(("jinja2" . "\\.html\\.j2\\'")))
  (add-to-list 'sp-ignore-modes-list 'web-mode)
  (add-to-list 'company-dabbrev-code-modes 'web-mode)
  (add-hook 'web-mode-hook (lambda ()
                             (when (equal web-mode-content-type "jsx")
                               (flycheck-mode +1)
                               (lsp))
                             )))

(use-package company-restclient)
(use-package restclient
  :config
  (add-to-list 'company-backends #'company-restclient))

(provide 'orary-web)
;;; orary-web.el ends here
