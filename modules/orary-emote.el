;;; orary-emote.el --- Emotes!
;;
;; Copyright (C) 2016 Ross Donaldson

;; Author: Ross Donaldson <gastove@gmail.com>
;; URL: https://github.com/Gastove/orary

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:

(require 'helm)

(defvar orary/emotes
  '(("Flip table: (╯°□°）╯︵ ┻━┻ " . ("( °_°） ┬──┬ " "(╯°□°）╯︵ ┻━┻ "))
    ("Angry table flip: (ノ□益□)ノ彡┻━┻ " . ("( °_°） ┬──┬ " "(-°□°）-┬──┬ " "(ノ□益□)ノ彡┻━┻ "))
    ("Flip it back: ┬──┬ ﻿ノ( ゜-゜ノ)" . ("┻━━┻    ( -.-  )" "┻━━┻  ノ( ゜-゜ノ)" "┬──┬    (  ^_^  )" "┬──┬ ﻿ノ( ゜-゜ノ)"))
    ("Flip two ┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻" . "┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻")
    ("Shades: (⌐■_■)" . ("( •_•)" "( •_•)>⌐■-■" "(⌐■_■)"))
    ("Shrug: ¯\_(ツ)_/¯" . "¯\_(ツ)_/¯")
    ("Music: ヽ(⌐■_■)ノ♪♬" . "ヽ(⌐■_■)ノ♪♬")
    ("Stuff to do: ᕕ(╯°□°)ᕗ" . "ᕕ(╯°□°)ᕗ")
    ("Stroll ᕕ( ᐛ )ᕗ" . "ᕕ( ᐛ )ᕗ")
    ("Love (●♡∀♡)" . "(●♡∀♡)")
    ("wut ⊙△⊙" . "⊙△⊙")
    ("Bring it (ง •̀_•́)ง" . "(ง •̀_•́)ง")
    ("Seriously? (¬_¬)" . "(¬_¬)")))

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
    (progn
     (kill-new emotes)
     (insert emotes))))

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
