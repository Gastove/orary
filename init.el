;;; init.el --- Init file
;;
;;; Commentary:
;;  Yep.  We got code.
;;; Code:

;; TODO: Electric colon for python, other modes
;; TODO: Make a unified var for saving info like place, history.
;; TODO: todo highlighting/font-lock
;; TODO: move multi-account Stuffz to work congifs so they don't clutter this up
;; TODO: start breaking things in to modules
;; TODO: Magit colors are all fucked
;; TODO: wtf is up with M-w
;; TODO: why don't I have persistent helm?

;;; Package Management

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; Packages it just doesn't make sense to manage with use-package
(defvar orary/packages '(dash
			 exec-path-from-shell
			 helm-ag
			 helm-projectile
			 org-plus-contrib
			 org-bullets
			 persp-projectile
			 rainbow-mode
			 rainbow-delimiters
			 use-package))

(defun orary/install-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun orary/install-packages ()
  (mapc #'orary/install-package orary/packages))

(orary/install-packages)

;;; Utilities
(require 'dash)   ;; Way better list manipulation functions
(require 'subr-x) ;; Gets us string-join

;;; OSX
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; OSX Modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Appearance
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'rhombus t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(which-function-mode -1)
(setq initial-buffer-choice t)

;; Alerts
(defun orary/visible-bell ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function #'ignore)
(setq visible-bell nil ring-bell-function 'orary/visible-bell)

;;; Core behaviors
;; Remember where we were in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/savefile/saved-places")


;; Remember command and file history
(require 'savehist)
(setq savehist-file "~/.emacs.d/savefile/savehist")

;; Custom File
(setq custom-file "~/.emacs.d/savefile/custom.el")
(load custom-file)

;; Packages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Package Loading and Configuration
(setq use-package-always-ensure t)

;; Org
(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(use-package org
  :config
  (add-hook 'org-mode-hook (lambda ()
			     ;; Make sure auto-fill-mode is on. Pretty
			     ;; much always need it.
			     (turn-on-auto-fill)
					;
			     ;; Dramatically improve company
			     ;; completion in org Org uses the
			     ;; `pcomplete' system; wire it up
			     (add-pcomplete-to-capf)
			     (require 'org-bullets)
			     (org-bullets-mode 1)))
  ;; TODO Keyword states:
  ;; > In-Progress states: BACKLOG, TODO, DOING, BLOCKED
  ;; > Finished states:    DONE, IMPOSSIBLE, CANCELLED
  (setq org-todo-keywords
	'((sequence "BACKLOG(k)")
	  (sequence "TODO(t)" "DOING(o)" "|" "DONE(d)")
	  (sequence "BLOCKED(b)" "|" "UNBLOCKED (u)" "CANCELLED(c)" "IMPOSSIBLE(i)"))

	org-todo-keyword-faces
	'(("TODO" . org-todo)
	  ("DOING" . org-todo)
	  ("BACKLOG" . org-todo)
	  ("BLOCKED" . org-warning)
	  ("CANCELLED" . org-done)
	  ("IMPOSSIBLE" . org-done)
	  ("DONE" . org-done))
	;; Config org export backends
	org-export-backends
	`(beamer
	  ascii
	  md
	  pandoc
	  gfm
	  deck
	  html
	  gnuplot)

	;; Export defaults: no table of contents, no numbered headers, don't convert ^
	;; or _ to superscripts
	org-export-with-section-numbers nil
	org-export-with-sub-superscripts nil
	org-export-with-toc nil

	;; Don't ask before src block eval
	org-confirm-babel-evaluate nil

	;; Refiling defaults
	org-refile-targets '((org-agenda-files :maxlevel . 5))
	org-refile-allow-creating-parent-nodes 'confirm

	org-agenda-text-search-extra-files '(agenda-archives)

	;; Hide org emphasis marks
	org-hide-emphasis-markers t

	;; Start indented
	org-startup-indented t

	;; Stop folding. Just... stop.
	org-startup-folded t

	;; Fontify inside code blocks
	org-src-fontify-natively t

	org-default-notes-file "~/Dropbox/org-docs/cotidienne.org"

	org-capture-templates
	`(("t" "Todo" entry (file+headline "" "General To-Dos")
	   "** TODO %?\n")
	  ("f" "File-Todo" entry (file+headline "" "General To-Dos")
	   "** TODO %?\n %i\n %A\n")
	  ("e" "Email" entry (file+headline "" "General To-Dos")
	   ,(string-join '("** TODO Reply to %:from re:%?"
			   "DEADLINE: <%(org-read-date nil nil \"+1d\")>"
			   "\%i"
			   "%a\n")
			 "\n"))
	  ("g" "Log" entry (file+headline "" "Log") "** email%?\n %l")))

  ;; Jump and Sparse-Tree contexts
  (push  '(org-goto . local) org-show-context-detail)
  (push '(tags-tree . local) org-show-context-detail)

  ;; Support for Babel Mode code blocks
  ;; NOTE: requires the addition of the org elpa repo!
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (java . t)
     (sh . t)
     (R . t)
     (scala . t)
     (scheme . t)
     (sql . t)))

  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))

;; Company
(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay .4
	company-minimum-prefix-length 2
	company-tooltip-limit 20))

;; Helm
(use-package helm
  :demand t
  :commands helm
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p           t
	helm-split-window-default-side        'below
	helm-split-window-in-side-p           t
	helm-buffers-fuzzy-matching           t
	helm-move-to-line-cycle-in-source     t
	helm-ff-search-library-in-sexp        t
	helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-c f" . helm-recentf)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("[tab]" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 :map helm-command-map
	 ("o" . helm-occur)
	 ("g" . helm-do-grep)
	 ("SPC" . helm-all-mark-rings)))

(unbind-key "s-m")
(use-package magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"
	magit-branch-read-upstream-first t
	magit-branch-arguments nil
	magit-push-arguments '("--set-upstream")
	magit-push-always-verify nil
	magit-revert-buffers t)
  :bind (("s-m m" . magit-status)
	 ("C-x g" . magit-status)
	 ("s-m l" . magit-log)
	 ("s-m b" . magit-blame)
	 ))

(use-package bookmark+
  :config
  (setq bookmark-default-file "~/Dropbox/emacs/gifs.bmk"
	bmkp-last-as-first-bookmark-file nil))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-completing-prompt)))

(use-package projectile
  :demand t
  :config
  (require 'persp-projectile)
  (require 'helm-projectile)
  (persp-mode)
  (helm-projectile-on)
  (projectile-global-mode +1)
  (setq projectile-completion-system 'helm
	projectile-switch-project-action 'projectile-dired)
  :bind-keymap ("s-p" . projectile-command-map)
  :bind (:map projectile-mode-map
	      ("C-c p p" . projectile-persp-switch-project)))

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  ;; Org-mode Pairs
  (sp-local-pair 'org-mode "~" "~" :wrap "C-~")
  (sp-local-pair 'org-mode "*" "*" :wrap "C-*")
  :bind (:map smartparens-mode-map
	      ("C-M-f" . sp-forward-sexp)
	      ("C-M-b" . sp-backward-sexp)
	      ("C-M-d" . sp-down-sexp)
	      ("C-M-a" . sp-backward-down-sexp)
	      ("C-S-d" . sp-beginning-of-sexp)
	      ("C-S-a" . sp-end-of-sexp)
	      ("C-M-e" . sp-up-sexp)
	      ("C-M-u" . sp-backward-up-sexp)
	      ("C-M-n" . sp-next-sexp)
	      ("C-M-p" . sp-previous-sexp)
	      ("C-M-k" . sp-kill-sexp)
	      ("C-M-w" . sp-copy-sexp)
	      ("M-s" . sp-splice-sexp)
	      ;; ("M-<delete>" . sp-unwrap-sexp)
	      ;; ("M-<backspace>" . sp-backward-unwrap-sexp)
	      ("C-)" . sp-forward-slurp-sexp)
	      ("C-}" . sp-forward-barf-sexp)
	      ("C-(" . sp-backward-slurp-sexp)
	      ("C-{" . sp-backward-barf-sexp)
	      ("C-M-<delete>" . sp-splice-sexp-killing-forward)
	      ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
	      ("C-S-<backspace>" . sp-splice-sexp-killing-around)
	      ("C-]" . sp-select-next-thing-exchange)
	      ("C-M-]" . sp-select-next-thing)
	      ("M-F" . sp-forward-symbol)
	      ("M-B" . sp-backward-symbol)))

(use-package unicode-fonts
  :demand t
  :config (unicode-fonts-setup))

(use-package smart-mode-line
  :config (sml/setup)
  (setq rm-blacklist '(" MRev"
		       " yas"
		       " Helm"
		       " company"
		       " guru"
		       " Pre"))

  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)

  ;; Java and scala package names are infinite and terrible; shorten them.
  (add-to-list 'sml/replacer-regexp-list '("^~/Code/" ":CODE:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/main/java/" ":SMJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/test/java/" ":STJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/main/scala/" ":SMS:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:CODE:\\(?:.*\\)\\{1,2\\}/src/test/scala/" ":STS:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:SM[JS]:com/urbanairship/\\(.*\\)/" ":M:\\1:") t)
  (add-to-list 'sml/replacer-regexp-list '("^:ST[JS]:com/urbanairship/\\(.*\\)/" ":T:\\1:") t)

  ;; Make sure I notice when I'm in
  (add-to-list 'rm-text-properties '(" Sp/s" 'face 'font-lock-warning-face)))

;;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'mu4e-contrib)
(require 'org-mu4e)


(setq mu4e-maildir "~/.Mail"
      mu4e-drafts-folder "/gastove@gmail.com/[Gmail].Drafts"
      mu4e-sent-folder   "/gastove@gmail.com/[Gmail].Sent Mail"

      ;; don't save message to Sent Messages, Gmail/IMAP/Offlineimap takes care of this
      mu4e-sent-messages-behavior 'delete

      ;; Let offlineimap's autorefresh handle getting new mail, but automatically re-index:
      mu4e-get-mail-command "offlineimap"

      ;; Make mu4e the default user agent
      mail-user-agent 'mu4e-user-agent

      ;; fetch mail every 5 mins
      mu4e-update-interval 300

      ;; Name, main email address
      user-mail-address "gastove@gmail.com"
      user-full-name  "Ross Donaldson"

      ;; Signature
      mu4e-compose-signature (concat "Cheers,\n"
				     "Ross\n")

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
      mu4e-compose-complete-only-after "2013-01-01"

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
(add-to-list 'mu4e-bookmarks `(,(string-join
				 '("flag:unread"
				   "AND date:today..now"
				   "NOT maildir:/ross@urbanairship.com/Githubs"
				   "NOT maildir:'/ross@urbanairship.com/Sales Deals'"
				   "AND m:/ross@urbanairship.com/INBOX")
				 " ")
			       "Today's work unreads" ?i))
(add-to-list 'mu4e-bookmarks `(,(string-join
				 '("flag:unread"
				   "AND m:/gastove@gmail.com/INBOX"
				   "AND date:today..now")
				 " ") "Today's Personal Unreads" ?h))
(add-to-list 'mu4e-bookmarks `(,(string-join
				 '("flag:unread"
				   "AND m:/gastove@gmail.com/INBOX"
				   "AND date:today..now"
				   "OR flag:unread"
				   "AND m:/ross@urbanairship.com/INBOX"
				   "AND date:today..now")
				 " ") "Today's Unreads" ?u))
(add-to-list 'mu4e-bookmarks `(,(string-join
				 '("m:/gastove@gmail.com/INBOX"
				   "AND date:10d..now"
				   "or m:/ross@urbanairship.com/INBOX"
				   "AND date:10d..now")
				 " ") "Working Mail" ?w))

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

(use-package wgrep
  :demand t)

(use-package wgrep-helm
  :demand t)

(use-package wgrep-ag
  :demand t
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (add-hook 'helm-ag-mode-hook 'wgrep-ag-setup))

;;; Languages
;; Lisp
(defun orary/elisp-defaults ()
  (eldoc-mode +1)
  (rainbow-mode +1)
  (rainbow-delimiters-mode-enable)
  (smartparens-strict-mode +1))

(add-hook 'emacs-lisp-mode-hook #'orary/elisp-defaults)

;; Python
(use-package elpy
  :config
  (elpy-enable)
  (require 'electric)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; Correctly fill python docstrings
(setq python-fill-docstring-style 'django)

(defun orary/python-mode-settings ()
  (setq-local electric-layout-rules
	      '((?: . (lambda ()
			(and (zerop (first (syntax-ppss)))
			     (python-info-statement-starts-block-p)
			     'after)))))
  (add-hook 'post-self-insert-hook
	    #'electric-layout-post-self-insert-function nil 'local))

(add-hook 'python-mode-hook #'orary/python-mode-settings)

;; nXML
(push 'nxml-mode sp-ignore-modes-list)
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (flyspell-mode-off)
	    (define-key prelude-mode-map (kbd "C-c C-i") 'nxml-balanced-close-start-tag-inline)))

;; Scala
(use-package ensime
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;;; Core editor behaviors
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

(defun orary/clean-up-buffer ()
  (interactive)
  (call-interactively 'untabify)
  (call-interactively 'indent-region)
  (whitespace-cleanup))

(add-hook 'before-save-hook #'orary/clean-up-buffer)

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

;; Keymaps!
(global-set-key [remap move-beginning-of-line]
		'orary/prelude-move-beginning-of-line)
(global-set-key (kbd "s-j") 'orary/join-below)
(global-set-key (kbd "C-c n") 'orary/clean-up-buffer)
(global-set-key (kbd "C-x |") 'orary/toggle-window-split)
(global-set-key (kbd "C-x j") 'orary/insert-iso-date)
(global-set-key (kbd "M-;") 'orary/comment-dwim-line)


;; Local work configs
(let ((work-configs (expand-file-name ".work.el" (getenv "HOME"))))
  (when (file-exists-p work-configs)
    (load-file work-configs)))

;; Last but not least, click on the server
(server-start)

(provide 'init)

;;; init.el ends here
