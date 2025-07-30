;;; orary-notify.el --- Notifications and Tracking in Orary -*- lexical-binding: t; -*-
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
;;  Wires up sauron + notify.el, so I actually know what the crap is going on
;;  around me.
;;; Code:

;; (require 'notify)

(use-package alert
  :config
  (if (eq system-type 'darwin)
      (setq alert-default-style 'growl)
    (setq alert-default-style 'libnotify)))

(use-package sauron
  :config
  (if (eq system-type 'darwin)
      (setq sauron-modules '(sauron-jabber sauron-erc sauron-org sauron-twittering sauron-notifications))
    (setq sauron-dbus-cookie 1))

  (setq sauron-separate-frame nil
        sauron-nick-insensitivity 300)

  (add-hook 'sauron-event-block-functions
            (lambda (origin prio msg &optional props)
              (and
               (eq 'twittering origin)
               (string-match "^[[:digit:]]* new tweets" msg))))
  (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)

  ;; Turn on sauron on emacs start
  ;; (sauron-start-hidden)
  ;; (define-key prelude-mode-map (kbd "C-M-?") 'sauron-toggle-hide-show)
  )


(provide 'orary-notify)
;;; orary-notify.el ends here
