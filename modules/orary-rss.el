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
  (setq elfeed-db-directory "~/Dropbox/elfeed"
        elfeed-feeds
        '(("http://feed43.com/2023102361452841.xml" comics evan-dahm)       ;; Riceboy
          ("http://limbero.org/jl8/rss/" comics jl8)                        ;; JL8
          ("http://sssscomic.com/ssss-feed.xml" comics ssss)                ;; Stand still, stay silent
          ("http://www.girlgeniusonline.com/ggmain.rss" comics girl-genius) ;; Girl Genius
          ("http://deadwinter.cc/dwrss.xml" comics deadwinter)              ;; Dead Winter
          ("http://www.giantitp.com/comics/oots.rss" comics oots)           ;; OOTS
          ("http://www.threepanelsoul.com/rss.php" comics tps)              ;; Three-Panel Soul
          ("http://www.lutherlevy.com/?feed=rss2" comics family-man)        ;; Family man
          ("https://jvns.ca/atom.xml" blog programming sre)                 ;; b0rk's blog
          ("http://oglaf.com/feeds/rss/" comics oglaf)                      ;; Oglaf
          ("http://ohumanstar.com/feed/" comics o-human-star)               ;; O Human Star
          ("http://www.smbc-comics.com/rss.php" comics smbc)                ;; Saturday Morning Breakfast Cereal
          ("http://dtrace.org/blogs/feed/" blog programming sre perf)       ;; Brendan Gregg's dtrace blog
          ("http://www.c.urvy.org/curvy_rss2.xml" comics curvy)             ;; Curvy
          ("https://gastove.com/blog/feed/atom.xml" blog programming self)  ;; moi
          ))
  :bind ("C-c q f" . elfeed))

(provide 'orary-rss)
;;; orary-rss.el ends here
