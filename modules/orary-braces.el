;;; orary-braces.el --- Tweaks for managing pairs of curlys
;;
;;; Commentary:
;;
;;; Code:

(defun orary/braces-open-pair ()
  "With cursor between a pair of curly braces, put point on a
correctly indented newline with the closing brace on the next
line."
  (newline-and-indent)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun orary/braces-open-newline ()
  (interactive)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))


(provide 'orary-braces)
;;; orary-braces.el ends here
