;;; orary-emote.el --- Emotes!
;;
;;; Commentary:
;;
;;; Code:

(require 'helm)

(defvar orary/emotes
  '(("Flip table: (╯°□°）╯︵ ┻━┻ " . "(╯°□°）╯︵ ┻━┻ ")
    ("Angry table flip: (ノ□益□)ノ彡┻━┻ " . "(ノ□益□)ノ彡┻━┻ ")
    ("Flip it back: ┬──┬ ﻿ノ( ゜-゜ノ)" . "┬──┬ ﻿ノ( ゜-゜ノ)")
    ("Shrug: ¯\_(ツ)_/¯" . "¯\_(ツ)_/¯")
    ("Music: ヽ(⌐■_■)ノ♪♬" . "ヽ(⌐■_■)ノ♪♬")
    ("Stuff to do: ᕕ(╯°□°)ᕗ" . "ᕕ(╯°□°)ᕗ")
    ("Stroll ᕕ( ᐛ )ᕗ" . "ᕕ( ᐛ )ᕗ")))

(defun orary/insert-emote ()
  "Allow for the selection and insertion of handy emotes."
  (interactive)
  (insert (helm :sources (helm-build-sync-source "Orary Emoji"
			  :candidates 'orary/emotes)
		:buffer "*Orary Emotes*")))

(provide 'orary-emote)
;;; orary-emote.el ends here
