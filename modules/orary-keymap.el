;;; orary-keymap.el --- Global keybindings for Orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Global keybindings in orary.
;;; Code:

(require 'orary-functions)

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
(define-key 'orary/toggle-map "h" #'hl-line-mode)
(define-key 'orary/toggle-map "l" #'linum-mode)
(define-key 'orary/toggle-map "i" #'orary/toggle-auto-indent)
(define-key 'orary/toggle-map "n" #'treemacs-select-window)
(define-key 'orary/toggle-map "p" #'persp-mode)
(define-key 'orary/toggle-map "w" #'which-function-mode)
(define-key 'orary/toggle-map "t" #'orary/toggle-window-split)
(define-key 'orary/toggle-map "?" #'sauron-toggle-hide-show)
(define-key 'orary/toggle-map "!" #'toggle-debug-on-error)

;;------------------------------- Global Keymap -------------------------------;;

(define-prefix-command 'orary/global-map)
(define-key ctl-x-map "m" 'orary/global-map)

(define-key 'orary/global-map (kbd "c t") #'orary/insert-signed-todo)
(define-key 'orary/global-map (kbd "c n") #'orary/insert-signed-note)
(define-key 'orary/global-map "d" #'orary/insert-iso-date)
(define-key 'orary/global-map "e" #'orary/insert-emote)
(define-key 'orary/global-map "f" #'make-frame-command)
(define-key 'orary/global-map "j" #'orary/pprint-json-in-new-buffer)
(define-key 'orary/global-map "n" #'orary/yank-visited-file-path)
(define-key 'orary/global-map "t" #'treemacs-select-window)
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

(define-prefix-command 'orary/multi-cursor-map)
(global-set-key (kbd "C->") 'orary/multi-cursor-map)

(define-key 'orary/multi-cursor-map "l" #'mc/edit-lines)
(define-key 'orary/multi-cursor-map ">" #'mc/mark-next-like-this)
(define-key 'orary/multi-cursor-map "<" #'mc/mark-previous-like-this)
(define-key 'orary/multi-cursor-map "b" #'mc/edit-beginnings-of-lines)
(define-key 'orary/multi-cursor-map "e" #'mc/edit-ends-of-lines)
(define-key 'orary/multi-cursor-map "a" #'mc/mark-all-like-this)



;;------------------------------------ LSP ------------------------------------;;
;; (define-prefix-command 'lsp-language-map)
;; (define-key ctl-x-map "l" 'lsp-language-map)

;; (define-key 'lsp-language-map "I" #'lsp-ui-imenu)
;; (define-key 'lsp-language-map (kbd "r r") #'lsp-rename)
;; (define-key 'lsp-language-map "a" #'lsp-execute-code-action)
;; (define-key 'lsp-language-map "d" #'lsp-describe-thing-at-point)
;; (define-key 'lsp-language-map (kbd "l s") #'lsp-lens-show)
;; (define-key 'lsp-language-map (kbd "l h") #'lsp-lens-hide)
;; (define-key 'lsp-language-map "f" #'lsp-format-buffer)

(provide 'orary-keymap)
;;; orary-keymap.el ends here
