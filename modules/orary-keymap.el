;;; orary-keymap.el ---
;;
;;; Commentary:
;;
;;; Code:

(global-set-key [remap move-beginning-of-line]
		'orary/prelude-move-beginning-of-line)
(global-set-key (kbd "s-j") 'orary/join-below)
(global-set-key (kbd "C-c n") 'orary/clean-up-buffer)
(global-set-key (kbd "C-x |") 'orary/toggle-window-split)
(global-set-key (kbd "C-x j") 'orary/insert-iso-date)
(global-set-key (kbd "M-;") 'orary/comment-dwim-line)
(define-key python-mode-map (kbd "C-c q r") 'orary/replace-double-quote-with-single)


(provide 'orary-keymap)
;;; orary-keymap.el ends here
