;;; orary-irc.el --- IRC Configuration for Orary
;;
;;; Commentary:
;; How does this even
;;; Code:

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

(require 'f)
(require 'dash)

;; Much of this is from Prelude

(erc-track-mode t)
;; truncate long irc buffers
(erc-truncate-mode +1)

;; enable spell checking
(erc-spelling-mode 1)

;; Let's set SO MANY DEFAULTS
(-let* ((erc-dir (f-expand "~/.erc"))
        (erc-log-dir (f-expand "logs" erc-dir)))
  (unless (f-exists? erc-dir)
    (f-mkdir erc-dir))
  (unless (f-exists? erc-log-dir)
    (f-mkdir erc-log-dir))
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#emacs"
                                       "#clojure"                                       
                                       "##fsharp")
                                      ("mozilla.org"
                                       "#rust-beginners"))

        erc-nick "gastove"

        ;; Interpret mIRC-style color commands in IRC chats
        erc-interpret-mirc-color t

        ;; The following are commented out by default, but users of other
        ;; non-Emacs IRC clients might find them useful.
        ;; Kill buffers for channels after /part
        erc-kill-buffer-on-part t
        ;; Kill buffers for private queries after quitting the server
        erc-kill-queries-on-quit t
        ;; Kill buffers for server messages after quitting the server
        erc-kill-server-buffer-on-quit t

        ;; open query buffers in the current window
        erc-query-display 'buffer

        ;; exclude boring stuff from tracking

        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")

        ;; logging
        erc-log-channels-directory erc-log-dir


        erc-save-buffer-on-part t

        ;; autoaway setup
        erc-auto-discard-away t
        erc-autoaway-idle-seconds 600
        erc-autoaway-use-emacs-idle t

        ;; utf-8 always and forever
        erc-server-coding-system '(utf-8 . utf-8)
        ))

;; Functions from Prelude, praise be.

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar erc-notify-timeout 10
  "Number of seconds that must elapse between notifications from
the same person.")

(defun erc-notify-allowed-p (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
  (unless delay (setq delay erc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick erc-notify-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) erc-notify-nick-alist)
      t)))

 (defun start-irc-freenode ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to connect to Freenode? ")
    (erc :server "irc.freenode.net" :port 6667 :nick erc-nick)))

(defun start-irc-mozilla ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to connect to Mozilla? ")
    (erc :server "irc.mozilla.org" :port 6667 :nick erc-nick)))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Asta la vista"))))

(provide 'orary-irc)
;;; orary-irc.el ends here
