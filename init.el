;;; init.el --- Init file
;;
;;; Commentary:
;;  Yep.  We got code.
;;; Code:

(require 'package)
(require 'subr-x)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

(unless (package-installed-p 'dash) (package-install 'dash))
(require 'dash)

;; Packages it just doesn't make sense to manage with use-package
(defvar orary/packages '(dash
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

;; OSX Modifier keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Appearance
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'rhombus t)

(scroll-bar-mode -1)
(which-function-mode -1)
(setq initial-buffer-choice t)
;; Alerts


;; Packages
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
  :ensure t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay .4
	company-minimum-prefix-length 2
	company-tooltip-limit 20))

;; Helm
(use-package helm
  :demand t
  :ensure t
  :commands helm
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-c f" . helm-recentf)
	 ("C-x C-f" . helm-find-files)
	 ("C-c h a" . helm-apropos)
	 ("C-c h i" . helm-semantic-or-imenu))
  :config
  (setq helm-split-window-in-side-p t
	helm-split-window-default-side 'below))

(use-package savehist
  :config (setq savehist-file "~/.emacs.d/savefile/savehist"))

(unbind-key "s-m")
(use-package magit
  :ensure t
;;  :config
  :bind (("s-m m" . magit-status)
	 ("C-x g" . magit-status)
	 ("s-m l" . magit-log)
	 ("s-m b" . magit-blame)
	 ))

(use-package projectile
  :ensure t
  :demand t
  :config
  (require 'persp-projectile)
  (require 'helm-projectile)
  (persp-mode)
  (helm-projectile-on)
  (projectile-global-mode +1)
  (setq projectile-completion-system 'helm)
  :bind-keymap ("s-p" . projectile-command-map)
  :bind (:map projectile-mode-map
	 ("C-c p p" . projectile-persp-switch-project)))

(use-package smartparens
  :ensure t
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
  :ensure t
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

;; Languages
(defun orary/elisp-defaults ()
  (eldoc-mode +1)
  (rainbow-mode +1)
  (rainbow-delimiters-mode-enable)
  (smartparens-strict-mode +1))

(add-hook 'emacs-lisp-mode-hook #'orary/elisp-defaults)

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
(global-set-key (kbd "M-;") `comment-dwim-line)


;; Last but not least, click on the server
(server-start)

(provide 'init)

;;; init.el ends here
