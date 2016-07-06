;;; orary-rss.el --- RSS Reading with Elfeed
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
;; I like RSS. Gimmie
;;; Code:

(use-package elfeed
  :config
  (setq elfeed-feeds
        '("http://feed43.com/2023102361452841.xml"     ;; Riceboy
          "http://limbero.org/jl8/rss/"                ;; JL8
          "http://sssscomic.com/ssss-feed.xml"         ;; Stand still, stay silent
          "http://www.girlgeniusonline.com/ggmain.rss" ;; Girl Genius
          "http://deadwinter.cc/dwrss.xml"             ;; Dead Winter
          "http://www.giantitp.com/comics/oots.rss"    ;; OOTS
          "http://www.lutherlevy.com/?feed=rss2"       ;; Family man
          ))
  :bind ("C-c q f" . elfeed))

(provide 'orary-rss)
;;; orary-rss.el ends here
