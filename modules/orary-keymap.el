;;; orary-keymap.el --- Global keybindings for Orary
;;
;;; Commentary:
;; Global keybindings in orary.
;;; Code:

(global-set-key [remap move-beginning-of-line]
        'orary/prelude-move-beginning-of-line)
(global-set-key (kbd "s-j") 'orary/join-below)
(global-set-key (kbd "M-;") 'orary/comment-dwim-line)
(global-set-key (kbd "C-c n") 'orary/clean-and-indent-buffer)

(global-set-key (kbd "C-c q t") 'orary/toggle-window-split)
(global-set-key (kbd "C-c q d") 'orary/insert-iso-date)
(global-set-key (kbd "C-c q e") 'orary/insert-emote)
(global-set-key (kbd "C-c q s") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c q j") 'orary/pprint-json-in-new-buffer)
(global-set-key (kbd "C-;") 'flyspell-auto-correct-previous-word)

(provide 'orary-keymap)
;;; orary-keymap.el ends here
