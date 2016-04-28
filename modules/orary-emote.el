;;; orary-emote.el --- Emotes!
;;
;;; Commentary:
;;
;;; Code:

(require 'helm)

(defvar orary/emotes
  (helm-build-sync-source "Orary Emoji"
    :candidates '(("Flip table: (╯°□°）╯︵ ┻━┻ " . "(╯°□°）╯︵ ┻━┻ ")
		  ("Angry table flip: (ノ□益□)ノ彡┻━┻ " . "(ノ□益□)ノ彡┻━┻ "))))

(defun orary/insert-emote ()
  (interactive)
  (insert (helm :sources 'orary/emotes
		:buffer "*Orary Emotes*")))

(provide 'orary-emote)
;;; orary-emote.el ends here
