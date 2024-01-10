;;; orary-functions.el --- What is Life Without Functions?
;;
;;; Commentary:
;; Utility functions and custom behaviors that don't live gracefully anywhere else.
;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'request)

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

(defun orary/edit-json-indirect (json-start json-end)
  (interactive "r")
  (-let [indirect (clone-indirect-buffer "*JSON Edit*" t t)]
    (with-current-buffer indirect
      (json-mode)
      (narrow-to-region json-start json-end)
      (json-pretty-print-buffer))))

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

(defun orary/clone (uri name dir)
  (-let ((default-directory (f-expand dir))
         (buf (get-buffer-create "*Git Clone*")))
    (call-process "git" nil buf nil "clone" uri name)))

(defun orary/clone-and-recall (repo dir)
  (-let* ((name (alist-get 'name repo))
          (uri (alist-get 'ssh_url repo))
          (projectile-name (concat (f-join dir name) "/")))
    (message "Cloning %s to %s" name projectile-name)
    (orary/clone uri name dir)
    (message "Adding to projectile...")
    (projectile-add-known-project projectile-name)
    (message "Done!")))

(defun orary/clone-from-github (prompt-for-dir)
  (interactive "P")
  (-let* ((dir (if prompt-for-dir
                   (read-directory-name "Clone in to dir: ")
                 "~/Code"))
          (repos (request "https://api.github.com/user/repos"
                   :parser 'json-read
                   :params '(("per_page" . "100"))
                   :sync 't
                   :headers `(("Authorization" . ,(concat "token " orary/github-oauth-token)))))
          (name-repo-mapping (-map (lambda (repo)
                                     (list (alist-get 'name repo) repo))
                                   (request-response-data repos))))
    (helm :sources (helm-build-sync-source "Available Repos"
                     :candidates name-repo-mapping
                     :action (lambda (repo)
                               (orary/clone-and-recall (car repo) dir)))
          :buffer "*Git Clone")))

(defun orary/insert-signed-comment (&optional extra)
  (orary/comment-dwim-line)
  (-let* ((whoami (user-login-name))
          (dt (format-time-string "%Y-%m-%d" (current-time)))
          (also (or extra ""))
          (sig (format "%s[%s|%s]" also whoami dt)))
    (insert sig)
    (end-of-line)))

(defun orary/insert-signed-todo ()
  (interactive)
  (orary/insert-signed-comment "TODO"))

(defun orary/insert-signed-note ()
  (interactive)
  (orary/insert-signed-comment "NOTE"))

(defvar-local orary/env-files '(".env.defaults" ".env" ".env.test"))

(defun orary/parse-bash-env-string (s)
  "Splits a bash-style export string S into a key-value pair."
  (s-split "=" s))

(defun orary/read-file (file-path)
  (with-temp-buffer
    (insert-file file-path)
    (s-split "\n" (buffer-string))))

(defun orary/read-env-file (file-path)
  (-let [lines (orary/read-file file-path)]
    (-filter (lambda (s) (and (not (s-starts-with-p "#" s))
                              (not (s-blank-p s))))
             lines)))

(defun orary/parse-bash-env-lines (lines)
  (-map #'orary/parse-bash-env-string lines))

(defun orary/load-file-to-env (file-path)
  (-let* ((lines (orary/read-env-file file-path))
          (parsed-lines (orary/parse-bash-env-lines lines)))
    (-each parsed-lines
      (lambda (kv-pair)
        (-let ((k (car kv-pair))
               (v (cadr kv-pair)))
          (message "Setting env value %s to %s" k v)
          (setenv k v)
          )))))

(defvar-local orary/env-already-loaded nil)

(defun orary/load-env (&optional reload)
  "For each of the env file names in orary/env-files, resolve to
the given project root and attempt to load k/v pairs out of
each."
  (interactive "P")
  (message "Got prefix arg: %s" reload)
  (if (or reload (not orary/env-already-loaded))
      (-let [dir (projectile-project-root)]
        (-each orary/env-files
          (lambda (file-name)
            (-let [file-path (f-expand file-name dir)]
              (if (f-exists-p file-path)
                  (orary/load-file-to-env file-path)))))
        (setq-local orary/env-already-loaded t)
        (message "Env files loaded"))
    (message "Skipping env load, already loaded")))

(defvar-local orary/rotatable-pairs
  '(("(" . ")")
    ("[" . "]")))

(setq-local orary/rotatable-pairs '(("(" . ")")
                                    ("[" . "]")))

(defun orary/rotate ()
  (interactive)
  (-let ((opening-pairs (s-join "\\|" (-map (-compose #'regexp-quote #'first) orary/rotatable-pairs)))
         (closing-pairs (s-join "\\|" (-map (-compose #'regexp-quote #'cdr) orary/rotatable-pairs)))
         (closing-pair-len (length orary/rotatable-pairs)))
    (-if-let* ((end-of-rotation
                (if (looking-back closing-pairs (- (point) closing-pair-len))
                    ;; If this matches, it matches the *end* of the string instead of the beginning
                    (-let [m (search-backward-regexp closing-pairs (- (point) closing-pair-len))]
                      (+ m 1))
                  (search-forward-regexp closing-pairs nil t)))
               (beginning-of-rotation (search-backward-regexp opening-pairs nil t))
               (working-text (buffer-substring beginning-of-rotation end-of-rotation)))

        ;; figure out whether we're replacing space with newline, or vice-versa
        (-let* ((going-to-from (if (looking-at "([A-Za-z0-9=_]+") 'space->newline 'newline->space))
                (look-for-re (if (eq going-to-from 'space->newline) ",\\s-+" ",\n\\s-*"))
                (replace-with-char (if (eq going-to-from 'space->newline) ",\n" ", "))
                (replacement-text nil))

          (-let ((first-cleanup (replace-regexp-in-string look-for-re replace-with-char working-text)))
            (setq replacement-text
                  (if (eq going-to-from 'space->newline)
                      (-let [newline-opening (replace-regexp-in-string "(" "(\n" first-cleanup)]
                        (replace-regexp-in-string ")" "\n)" newline-opening))
                    (-let [cleanup-opening-paren (replace-regexp-in-string "(\n\\s-+" "(" first-cleanup)]
                      (replace-regexp-in-string "\n)" ")" cleanup-opening-paren))))

            (delete-region beginning-of-rotation end-of-rotation)
            (goto-char beginning-of-rotation)
            (insert replacement-text)
            (indent-region beginning-of-rotation (point))))

      (message "Nothing to rotate"))
    ))

(defun orary/yank-commented ()
  "Yank the current first entry of the kill ring, inserting at
point and commenting according to the syntax of the current major
mode."
  (interactive)
  (-let [init (point)]
    (yank)
    (comment-region init (point))))

;;--------------------------------New Projects----------------------------------

(defun orary/create-new-project (DIR &optional ARG)
  (interactive "DDirectory:\nP")
  (unless (f-exists? DIR)
    (-let [dirs (s-split "/" DIR)]
      (apply #'f-mkdir dirs)))
  (save-excursion
    (magit-init DIR)
    (projectile-add-known-project DIR))
  (unless ARG
    (projectile-switch-project-by-name DIR)))

;;---------------------------Window Splitting Tools-----------------------------
(defun orary/three-windows ()
  (-let (left-window middle-window right-window)
    (setq left-window (frame-selected-window))
    (setq middle-window (split-window-right))
    (select-window middle-window)
    (setq right-window (split-window-right))
    (balance-windows)
    ;; if (called-interactively-p)
    (list :left-window left-window :middle-window middle-window :right-window right-window)))

(defun orary/split-into-balanced-thirds ()
  (interactive)
  (orary/three-windows)
  (message "Split!"))

(defun orary/split-window-unbalanced-thirds ()
  "Split the current window so the left is 2/3rds of the width,
1/3rd on the right."
  (interactive)
  (-let ((left-window (frame-selected-window))
         (right-window (split-window nil (/ (display-mm-width) 3) 'right)))
    (if (called-interactively-p)
        (message "Split!")
      (list :left-window left-window :right-window right-window))))

(defun orary/infer-screen-type-from-size ()
  "Infer the type of the current display, based on its size."
  (cond
   ((> 400 (display-mm-width)) 'laptop)
   ((> 1920 (display-mm-width)) 'gigantic-monitor)
   (t 'monitor)
   ))

(defun orary/get-new-workspace-spec (screen-type)
  (pcase screen-type
    ('laptop (lambda () (message "The laptop")))
    ('monitor (lambda () (message "monitor")))
    ('gigantic-monitor (lambda () (message "gigantic-monitor")))))

;; (orary/infer-screen-type-from-size)


;; (setq test-windows (orary/three-windows))

;; (-let ((curr (frame-selected-window))
;;        (win (plist-get test-windows :right-window)))
;;   (select-window win)
;;   (crux-create-scratch-buffer)
;;   (select-window curr))

(defun orary/init-main-workspace ()
  "Open up the org agenda, my notes, a scratch buffer."
  (interactive)
  (-let [middle-window (split-window-right)]
    (select-window middle-window)
    (find-file (f-expand "~/Documents/work.org"))
    (-let ((right-window (split-window-right))
           (org-agenda-window-setup 'current-window))
      (select-window right-window)
      (org-agenda-list))
    (balance-windows)
    (select-window middle-window)))


(defun orary/byte-recompile-everything ()
  "Byte-recompile every single package and dependency; useful if
  something gets out of sync and throws incomprehensible errors,
  like:

  smartparens/:catch: Symbol’s value as variable is void: <"
  (interactive)
  (byte-recompile-directory (f-expand "~/.emacs.d/elpa") 0 t))

;;-------------------------------- Repeat Keys --------------------------------;;
;; TODO: to make this work, I need to set a local variable -- something that
;; outlives each given call to the repeating function.
(defun orary/insert-key-seq (base mod-one mod-two &optional multiplier)
  (-let* ((multi (case nil
                   (1 1)
                   (4 2)
                   (otherwise 1)))
          (base (s-repeat multi base))
          (right (s-concat base mod-one))
          (left (s-concat mod-two base)))
    ;; (message "Multi is %s, right is %s, left is %s" multi right left)
    (cond ((re-search-backward right (- (point) (length right)) t)
           (replace-match left))
          ((re-search-backward left (- (point) (length left)) t)
           (replace-match base))
          ((thing-at-point-looking-at base 1) (insert mod-one))
          (:else (insert base)))))

;;---------------------- Misc utilities of various kinds ----------------------;;

(defun orary/base64-encode-region ()
  "bas64-encode-region with no line breaks."
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun orary/base64-decode-region ()
  "bas64-encode-region with no line breaks."
  (interactive)
  (base64-decode-region (mark) (point)))

(defun orary/yank-visited-file-path (prefix)
  "Put the absolute path of the currently visited file in the kill
ring. With prefix ARG, only put the relative path in the kill
ring. With a double ARG, only put the name of the file."
  (interactive "p")
  (-let* ((full-path (buffer-file-name))
          (to-kill (pcase prefix
                     (1 full-path)
                     (4 (f-relative full-path (projectile-project-root)))
                     (16 (f-relative full-path default-directory))
                     (_ full-path))))

    (message to-kill)
    (kill-new to-kill)
    ))

(defun orary/find-fsharp-test-dir (impl-dir-abs-path)
  (-let* ((impl-dir-name (s-chop-suffix "/" (-last-item (f-split impl-dir-abs-path))))
          (src-dir-name (f-parent impl-dir-abs-path))
          (test-dir (-first (lambda (dir)
                               (and (s-contains? impl-dir-name dir)
                                    (or (s-ends-with? "Test" dir)
                                        (s-ends-with? "Tests" dir))))
                             (f-directories src-dir-name))))
    test-dir))

;; (orary/find-fsharp-test-dir "/home/gastove/Code/cookbook/src/Fluhg")

(defun orary/bare-file-name ()
  "Extract the filename, without extension, from the current
buffer. E.g., if the buffer is visiting
/home/gastove/beep/boop/bonk/Flarb.fs, return Flarb."
  (interactive)
  (->> (buffer-file-name)
       (file-name-nondirectory)
       (file-name-sans-extension)))

(provide 'orary-functions)
;;; orary-functions.el ends here
