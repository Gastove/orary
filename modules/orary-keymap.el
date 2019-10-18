;;; orary-keymap.el --- Global keybindings for Orary
;;
;;; Commentary:
;; Global keybindings in orary.
;;; Code:

;;---------------------------------- Unsets ----------------------------------;;
;; Unset compose-mail so we can use this binding for the global map instead
(global-unset-key (kbd "C-x m"))
;; Unset count-lines for LSP
(global-unset-key (kbd "C-x l"))

;;-------------------------------- Toggle Map --------------------------------;;

(define-prefix-command 'orary/toggle-map)

(define-key ctl-x-map "t" 'orary/toggle-map)

(define-key 'orary/toggle-map "a" #'auto-fill-mode)
(define-key 'orary/toggle-map "f" #'toggle-frame-fullscreen)
(define-key 'orary/toggle-map "F" #'follow-mode)
(define-key 'orary/toggle-map "l" #'linum-mode)
(define-key 'orary/toggle-map "i" #'orary/toggle-auto-indent)
(define-key 'orary/toggle-map "n" #'treemacs)
(define-key 'orary/toggle-map "w" #'which-function-mode)
(define-key 'orary/toggle-map "t" #'orary/toggle-window-split)
(define-key 'orary/toggle-map "?" #'sauron-toggle-hide-show)
(define-key 'orary/toggle-map "!" #'toggle-debug-on-error)

;;------------------------------- Global Keymap -------------------------------;;

(define-prefix-command 'orary/global-map)
(define-key ctl-x-map "m" 'orary/global-map)

(define-key 'orary/global-map "c" #'orary/insert-signed-comment)
(define-key 'orary/global-map "d" #'orary/insert-iso-date)
(define-key 'orary/global-map "e" #'orary/insert-emote)
(define-key 'orary/global-map "j" #'orary/pprint-json-in-new-buffer)
(define-key 'orary/global-map "w" #'helm-world-time)

;;--------------------------- Other global commands ---------------------------;;

(global-set-key [remap move-beginning-of-line]
                'orary/prelude-move-beginning-of-line)
(global-set-key (kbd "s-j") 'orary/join-below)
(global-set-key (kbd "M-;") 'orary/comment-dwim-line)
(global-set-key (kbd "C-c n") 'orary/clean-and-indent-buffer)
(global-set-key (kbd "C-;") 'flyspell-auto-correct-previous-word)
(global-set-key (kbd "C-S-y") 'orary/yank-commented)
(global-set-key (kbd "C-c C-i") #'company-complete)

;;------------------------------------ LSP ------------------------------------;;
(define-prefix-command 'lsp-language-map)
(define-key ctl-x-map "l" 'lsp-language-map)

(define-key 'lsp-language-map "I" #'lsp-ui-imenu)
(define-key 'lsp-language-map (kbd "r r") #'lsp-rename)
(define-key 'lsp-language-map "a" #'lsp-execute-code-action)
(define-key 'lsp-language-map "d" #'lsp-describe-thing-at-point)
(define-key 'lsp-language-map (kbd "l s") #'lsp-lens-show)
(define-key 'lsp-language-map (kbd "l h") #'lsp-lens-hide)
(define-key 'lsp-language-map "f" #'lsp-format-buffer)

(provide 'orary-keymap)
;;; orary-keymap.el ends here
