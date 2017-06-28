;;; orary-core.el --- Core behaviors for Orary
;;
;;; Commentary:
;; Primary configuration for `orary' -- save files, libs we want everywhere.
;;; Code:

(require 'f)
(require 'dash)

(add-to-list 'load-path (f-expand "vendor/" orary/orary-root-dir))

(defvar orary/save-root (f-expand "savefile" orary/orary-root-dir))

;; Custom location:
(-let [custom-file-path (f-expand "custom.el" orary/save-root)]
  ;; `custom-file' has to exist or Emacs gets crabby
  (unless (f-exists? custom-file-path)
    (f-touch custom-file-path))
  (setq custom-file custom-file-path))
;; Seems like this never gets loaded unless I load it? Neet.
(load custom-file)

;; A good place for abbrevs
(setq abbrev-file-name (f-expand "abbrev_defs" orary/save-root)
      save-abbrevs 'silently)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Remember where we were in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (f-expand "saved-places" orary/save-root))


;; Remember command and file history
(require 'recentf)
(setq recentf-save-file (f-expand "recentf" orary/save-root)
      recentf-max-menu-items 50
      recentf-max-saved-items 100
      recentf-auto-cleanup 'never)

;; Lifted with gratitude from Prelude
(defun orary/recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list orary/save-root package-user-dir)))))

(add-to-list 'recentf-exclude 'orary/recentf-exclude-p)
(recentf-mode 1)

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

;; Handy timezones
(require 'time)
(setq display-time-world-list
      '(("America/Los_Angeles" "Pacific")
        ("America/Denver" "Mountain")
        ("America/Mexico_City" "Central")
        ("America/New_York" "Eastern")
        ("Atlantic/Reykjavik" "Iceland")
        ("UTC" "UTC")
        ("Europe/Paris" "Paris, France")
        ("Asia/Irkutsk" "Baikal")))

;;---------Whitespace Management and Cleanup, Tabs, Line Length, et al----------
;; Repeat after me: fuck tabs.
(set-default 'indent-tabs-mode nil)

;; 8 is a dumb number of spaces for a tab to default to.
(setq-default tab-width 4)

;; It's 2016, Emacs.
(setq sentence-end-double-space nil)

;; When do we not want this? Never? I think never.
(global-key-binding (kbd "RET") 'newline-and-indent)

;; Default line length
(setq-default fill-column 80)

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

;; bbatsov's crux, for all manner of handy things
(use-package crux
  :demand t
  :bind (("C-k" . crux-smart-kill-line)
         ("C-c s" . crux-swap-windows)))

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

;;----------------------------------Auto Save------------------------------------

;; Heavily inspired by Prelude

(defun orary/auto-save-command ()
  "Auto-saves, if it should (which it usually should)."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defadvice switch-to-buffer (before switch-to-buffer-auto-save activate)
  (orary/auto-save-command))

(defadvice other-window (before other-window-auto-save activate)
  (orary/auto-save-command))

(defadvice windmove-up (before windmove-up-auto-save activate)
  (orary/auto-save-command))

(defadvice windmove-down (before windmove-down-auto-save activate)
  (orary/auto-save-command))

(defadvice windmove-left (before windmove-left-auto-save activate)
  (orary/auto-save-command))

(defadvice windmove-right (before windmove-right-auto-save activate)
  (orary/auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'orary/auto-save-command)

;; Clean up old buffers
(require 'midnight)

;;--------------------------------------Tramp----------------------------------------
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-default-user "gastove")
(setq tramp-verbose 6)

;;--------------------------------------Window Splitting----------------------------------------
(defun orary/split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or
     (and (window-splittable-p window t)
          ;; Split window horizontally.
          (with-selected-window window
            (split-window-right)))
     (and (window-splittable-p window)
          ;; Split window vertically.
          (with-selected-window window
            (split-window-below)))
     (and (eq window (frame-root-window (window-frame window)))
          (not (window-minibuffer-p window))
          ;; If WINDOW is the only window on its frame and is not the
          ;; minibuffer window, try to split it vertically disregarding
          ;; the value of `split-height-threshold'.
          (let ((split-height-threshold 0))
            (when (window-splittable-p window)
              (with-selected-window window
                (split-window-below))))))))

(setq split-window-preferred-function 'orary/split-window-sensibly)

(provide 'orary-core)
;;; orary-core.el ends here
