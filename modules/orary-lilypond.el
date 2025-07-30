;;; orary-lilypond.el --- Lilypond Music Programming in Orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; If available, load and configure lilypond <3
;;; Code:

(require 'dash)
(require 'f)

(when (f-exists? "/Applications/LilyPond.app/")
  (-let [lilypath "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/"]
    (add-to-list 'load-path lilypath)
    (load (f-expand "lilypond-init.el" lilypath))))

(provide 'orary-lilypond)
;;; orary-lilypond.el ends here
