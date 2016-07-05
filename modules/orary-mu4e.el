;;; orary-mu4e.el --- mu4e integration
;;
;;; Commentary:
;;
;;; Code:

;;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'mu4e-contrib)
(require 'org-mu4e)
(require 's)

(setq mu4e-maildir "~/.Mail"
      mu4e-drafts-folder "/gastove@gmail.com/[Gmail].Drafts"
      mu4e-sent-folder   "/gastove@gmail.com/[Gmail].Sent Mail"

      ;; don't save message to Sent Messages, Gmail/IMAP/Offlineimap takes care of this
      mu4e-sent-messages-behavior 'delete

      ;; Don't include me in reply-alls
      mu4e-compose-dont-reply-to-self t

      ;; Let offlineimap's autorefresh handle getting new mail, but automatically re-index:
      mu4e-get-mail-command "true"

      ;; Make mu4e the default user agent
      mail-user-agent 'mu4e-user-agent

      ;; fetch mail every 5 mins
      mu4e-update-interval 300

      ;; Name, main email address
      user-mail-address "gastove@gmail.com"
      user-full-name  "Ross Donaldson"

      ;; Signature
      mu4e-compose-signature "Ross Donaldson\nData Scientist, Urban Airship\nTechnologist, Reed College Software Design Studio\nhttp://csv.rodeo"

      ;; ISO date format for headers
      mu4e-headers-date-format "%Y-%m-%d"

      ;; Convert HTML emails to nicely readable text
      mu4e-html2text-command 'mu4e-shr2text

      ;; Technically not mu4e, but shr is hard to read:
      shr-color-visible-luminance-min 80

      ;; If the same email is in two different folders, don't show it twice
      ;; in search results
      mu4e-headers-skip-duplicates t

      ;; Show images inline
      mu4e-view-show-images t

      ;; Silence the damn minibuffer updates
      mu4e-hide-index-messages t

      ;; Filter autocomplete addresses more intelligently
      mu4e-compose-complete-only-after "2014-01-01"

      ;; Capture better
      org-mu4e-link-query-in-headers-mode nil)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; configuration for sending mail
(setq message-send-mail-function
      ;; Send via postfix
      'sendmail-send-it

      ;; By default, the gnus composer (which mu4e uses also) doesn't correctly
      ;; write FROM headers. Let's fix that. (Required to correctly dispatch to
      ;; multiple sending accounts with postfix.)
      mail-specify-envelope-from t
      mail-envelope-from 'header)

;; Message Composition settings
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (whitespace-mode -1)))

;; The mu4e interface
;; Single-character shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gastove@gmail.com/INBOX"               . ?i)
        ("/gastove@gmail.com/[Gmail].Important"   . ?I)
        ("/gastove@gmail.com/[Gmail].Sent Mail"   . ?s)))


;; Tweak bookmarked queries
(add-to-list 'mu4e-bookmarks `(,(s-join " "
                                        '("flag:unread"
                                          "AND date:today..now"
                                          "NOT maildir:/ross@urbanairship.com/Githubs"
                                          "NOT maildir:'/ross@urbanairship.com/Sales Deals'"
                                          "AND m:/ross@urbanairship.com/INBOX"))
                               "Today's work unreads" ?i))
(add-to-list 'mu4e-bookmarks `(,(s-join " "
                                        '("flag:unread"
                                          "AND m:/gastove@gmail.com/INBOX"
                                          "AND date:today..now"))
                               "Today's Personal Unreads" ?h))
(add-to-list 'mu4e-bookmarks `(,(s-join " "
                                        '("flag:unread"
                                          "AND m:/gastove@gmail.com/INBOX"
                                          "AND date:today..now"
                                          "OR flag:unread"
                                          "AND m:/ross@urbanairship.com/INBOX"
                                          "AND date:today..now"))
                               "Today's Unreads" ?u))
(add-to-list 'mu4e-bookmarks `(,(s-join " "
                                        '("m:/gastove@gmail.com/INBOX"
                                          "AND date:10d..now"
                                          "or m:/ross@urbanairship.com/INBOX"
                                          "AND date:10d..now"))
                               "Working Mail" ?w))

(defvar my-mu4e-account-alist
  '(("gastove@gmail.com"
     (mu4e-drafts-folder "/gastove@gmail.com/[Gmail].Drafts")
     (mu4e-sent-folder   "/gastove@gmail.com/[Gmail].Sent Mail")
     (user-mail-address "gastove@gmail.com"))
    ("ross@urbanairship.com"
     (mu4e-drafts-folder "/ross@urbanairship.com/[Gmail].Drafts")
     (mu4e-sent-folder   "/ross@urbanairship.com/[Gmail].Sent Mail")
     (user-mail-address "ross@urbanairship.com"))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(provide 'orary-mu4e)
;;; orary-mu4e.el ends here
