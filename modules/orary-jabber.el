;;; orary-jabber.el --- Jabber wiring-up for Orary
;;
;;; Commentary:
;; Get you on a GCHAT!
;;; Code:

(use-package jabber
  :config
  (setq jabber-account-list
    '(("gastove@gmail.com"
       (:network-server . "talk.google.com")
       (:connection-type . ssl)
       (:port . 5223)))
    jabber-history-enabled t
    jabber-vcard-avatars-retrieve nil
    jabber-chat-buffer-show-avatar nil
    jabber-roster-show-bindings nil
    jabber-show-offline-contacts nil
    jabber-auto-reconnect t
    jabber-roster-show-title nil
    jabber-alert-presence-message-function 'jabber-presence-only-chat-open-message
    jabber-use-global-history t
    jabber-global-history-filename (locate-user-emacs-file "var/jabber.log"))
  (add-hook 'jabber-chat-mode-hook
        (lambda ()
          (turn-on-flyspell)
          ;; (emoji-cheat-sheet-plus-display-mode)
          (toggle-word-wrap 1)
          (if truncate-lines (toggle-truncate-lines)))))

(provide 'orary-jabber)
;;; orary-jabber.el ends here
