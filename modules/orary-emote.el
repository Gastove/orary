;;; orary-emote.el --- Emotes!
;;
;;; Commentary:
;;
;;; Code:

(require 'helm)

(defvar orary/emotes
  '(("Flip table: (╯°□°）╯︵ ┻━┻ " . ("( °_°） ┬──┬ " "(╯°□°）╯︵ ┻━┻ "))
    ("Angry table flip: (ノ□益□)ノ彡┻━┻ " . ("( °_°） ┬──┬ " "(-°□°）-┬──┬ " "(ノ□益□)ノ彡┻━┻ "))
    ("Flip it back: ┬──┬ ﻿ノ( ゜-゜ノ)" . ("┻━━┻    ( -.-  )" "┻━━┻  ノ( ゜-゜ノ)" "┬──┬    (  ^_^  )" "┬──┬ ﻿ノ( ゜-゜ノ)"))
    ("Flip two ┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻" . "┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻")
    ("Shrug: ¯\_(ツ)_/¯" . "¯\_(ツ)_/¯")
    ("Music: ヽ(⌐■_■)ノ♪♬" . "ヽ(⌐■_■)ノ♪♬")
    ("Stuff to do: ᕕ(╯°□°)ᕗ" . "ᕕ(╯°□°)ᕗ")
    ("Stroll ᕕ( ᐛ )ᕗ" . "ᕕ( ᐛ )ᕗ")
    ("Love (●♡∀♡)" . "(●♡∀♡)")
    ("wut ⊙△⊙" . "⊙△⊙")
    ("Bring it (ง •̀_•́)ง" . "(ง •̀_•́)ง")))

(defun orary/emote-insert-emote (emotes)
  "Entertains you with messaged emotes, then copies the final
emote from EMOTES to the kill ring and inserts it."
  (if (listp emotes)
      (progn
        (dolist (emote emotes)
         (message emote)
         (sleep-for 0 500))
       (let ((final (car (last emotes))))
         (kill-new final)
         (insert final)))
    (kill-new emotes)
    (insert emotes)))

(defvar orary/emote-actions
  (helm-make-actions "Insert + copy to kill ring" 'orary/emote-insert-emote
                     "Copy to kill ring" 'kill-new))

(defun orary/insert-emote ()
  "Allow for the selection and insertion of handy emotes."
  (interactive)
  (helm :sources (helm-build-sync-source "Orary Emoji"
                   :candidates 'orary/emotes
                   :action 'orary/emote-actions)
        :buffer "*Orary Emotes*"))

(provide 'orary-emote)
;;; orary-emote.el ends here
