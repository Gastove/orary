;;; orary-functions.el --- What is Life Without Functions?
;;
;;; Commentary:
;; Utility functions and custom behaviors that don't live gracefully anywhere else.
;;; Code:

;; With gratitude to Bozhidar Batsov
(defun orary/prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defun orary/join-below ()
  (interactive)
  (delete-indentation 1))

(defun orary/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun orary/insert-iso-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun orary/comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.

If no region is selected and current line is not blank
	and we are not at the end of the line, then comment
	current line.  Replaces default behaviour of
	`comment-dwim', when it inserts comment at the end of the
	line.  With an argument, passes ARG to `comment-dwim'"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun orary/replace-double-quote-with-single ()
  "This function replaces a thing wrapped in double-quotes with
the same thing wrapped in single-quotes.  For my next trick:
rotate quotes!"
  (interactive)
  (save-excursion
    (let (p1 p2 rep-char)
      (re-search-backward "[\"']" nil t)
      (setq rep-char (if (looking-at "\"") "'" "\""))
      (delete-char 1)
      (insert rep-char)
      (forward-char 1)
      (re-search-forward "[\"']" nil t)
      (backward-char 1)
      (delete-char 1)
      (insert rep-char))))

(provide 'orary-functions)
;;; orary-functions.el ends here
