;;; orary-core.el --- Core behaviors for Orary
;;
;;; Commentary:
;; Primary configuration for `orary' -- save files, libs we want everywhere.
;;; Code:

(require 'f)
(require 'dash)

(defvar orary/save-root (f-expand "savefile" orary/orary-root-dir))

;; Custom location:
(-let [custom-file-path (f-expand "custom.el" orary/save-root)]
  ;; `custom-file' has to exist or Emacs gets crabby
  (unless (f-exists? custom-file-path)
    (f-touch custom-file-path))
  (setq custom-file custom-file-path))
;; Seems like this never gets loaded unless I load it? Neet.
(load custom-file)

;; Remember where we were in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (f-expand "saved-places" orary/save-root))


;; Remember command and file history
(require 'savehist)
(setq savehist-file (f-expand "savehist" orary/save-root))

;;; auto-save
;; Stop sticking foo~ files everywhere, jfc
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(f-expand "autosave" orary/save-root)))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Don't make me type "yes" all the time
(defalias 'yes-or-no-p 'y-or-n-p)

;;---------Whitespace Management and Cleanup, Tabs, Line Length, et al----------
;; Repeat after me: fuck tabs.
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)

;; It's 2016, Emacs.
(setq sentence-end-double-space nil)

;; When do we not want this? Never? I think never.
(global-key-binding (kbd "RET") 'newline-and-indent)

;; Default line length
(setq-default fill-column 79)

;;------------------------------Core enhancements--------------------------------
;;; Improve on built-in bookmarks, dired
;; Enhance... enhance...
(use-package bookmark+
  :config
  (setq bookmark-default-file "~/Dropbox/emacs/gifs.bmk"
        bmkp-last-as-first-bookmark-file nil))

;; ...enhance
(use-package dired+
  :init (setq diredp-hide-details-initially-flag nil))

;;---------------------------------Navigation-----------------------------------
;; Go directly to the window you want, by number.
(use-package ace-window
  :demand t
  :bind (("C-c w" . ace-window)
         ("s-w" . ace-window)))

(use-package avy
  :config (setq avy-background t
                avy-style 'at-full)
  :bind ("C-c j" . avy-goto-word-or-subword-1))

;; Window movement by arrow key
(require 'windmove)
(windmove-default-keybindings)

;; Zopping is like zapping, but much more flexible and neat.
(use-package zop-to-char
  :demand t
  :bind ("M-z" . zop-to-char))

(provide 'orary-core)
;;; orary-core.el ends here
