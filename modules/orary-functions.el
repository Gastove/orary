;;; orary-functions.el --- What is Life Without Functions?
;;
;;; Commentary:
;; Utility functions and custom behaviors that don't live gracefully anywhere else.
;;; Code:

(require 'dash)
(require 'f)
(require 's)

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

(defun orary/elisp-unbind-symbol (&optional sym)
  "If this function is called with symbol SYM, unbind it.
Otherwise, if `thing-at-point' is a symbol, unbind it. Note that
this relies on `symbol-at-point', which can be very generous in
its notion of what a symbol is."
  (interactive)
  (if (and sym (symbolp sym))
      (makunbound sym)
    (-if-let (sym (and (boundp (symbol-at-point))
                       (symbol-at-point)))
        (progn
          (makunbound sym)
          (message (format "Unbound symbol '%s'" sym)))
      (message "thing-at-point is not a symbol! Ignoring."))))

(defun orary/load-projectile-projects (code-root)
  "Read every subdirectory in CODE-ROOT; if a subdirectory is a
  valid projectile project, add it as a known project."
  (interactive "D")
  (-let [cnt 0]
    (dolist (d (f-directories code-root))
      (unless (equal 'none (projectile-project-vcs d))
        ;; Projectile thinks project dirs end in /; f does not.
        (projectile-add-known-project (s-append "/" d))
        (message (format "Added %s to known projects" d))
        (setq cnt (1+ cnt))))
    (if (eq cnt 0)
        (message "No projects found")
      (message (format "Added %s projects" cnt)))))

(defun orary/pprint-json-in-new-buffer (json-start json-end)
  "If the region delimited by JSON-START and JSON-END encompases
valid JSON, read that JSON in to a new buffer and pretty print
it."
  (interactive "r")
  (condition-case nil
      (-let ((src-buf (current-buffer))
             (buf (get-buffer-create "*json-pretty-print*")))
        (with-current-buffer buf
          (json-mode)
          (insert-buffer-substring-no-properties src-buf
                                                 json-start
                                                 json-end)
          (json-pretty-print-buffer)
          (goto-char (point-min)))
        (switch-to-buffer-other-window buf))
    (json-readtable-error (message "Region was not on valid JSON."))))

(defun orary/toggle-auto-indent ()
  "Toggle the current value of orary/disable-auto-indent."
  (interactive)
  (setq orary/disable-auto-indent (not orary/disable-auto-indent))
  (message "Auto-indent now %s" (if orary/disable-auto-indent "disabled" "enabled")))

(provide 'orary-functions)
;;; orary-functions.el ends here
