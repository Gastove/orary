;;; orary-programming.el --- Defaults for Programming Language Modes
;;
;;; Commentary:
;; Tools like smartparens, flycheck, etc.
;;; Code:

(require 'dash)

;; Auto-wrap comments. Like an adult.
(require 'newcomment)
(auto-fill-mode +1)
(setq comment-auto-fill-only-comments 1)
(setq auto-fill-function #'do-auto-fill)

(use-package rainbow-delimiters)

(define-minor-mode orary/programming-mode "Programming behaviors for orary")

(defun orary/important-comments ()
  "Some source code comments are important; make them shine.

Currently catches: FIX(ME)?, TODO, NOTE."
  (font-lock-add-keywords
   nil
   '(("\\<\\(\\(TODO\\|FIX\\(ME\\)?\\)\\)" 1 font-lock-warning-face t)
     ("\\<\\(NOTE\\)" 1 font-lock-string-face t))))

(use-package ethan-wspace
  :commands ethan-wspace-clean-all
  :diminish ethan-wspace-mode
  :demand t
  :init
  (global-ethan-wspace-mode 1)
  (setq-default require-final-newline nil)
  (setq-default mode-require-final-newline nil)
  :config
  (setq ethan-wspace-warned-mode-require-final-newline t))

;; ggtags is *so fiddly* to get working; gonna lean on other tools for now.
;; (use-package ggtags)

(defvar orary/gg-tags-modes
  '(c-mode 'c++-mode java-mode emacs-lisp-mode python-mode))

(defvar orary/indent-sensitive-modes
  '(conf-mode yaml-mode scala-mode purescript-mode org-mode makefile-gmake-mode
              markdown-mode sql-mode ein:notebook-python-mode dockerfile-mode picture-mode
              elm-mode snippet-mode fsharp-mode))

(defvar orary/disable-auto-indent nil)
(make-variable-buffer-local 'orary/disable-auto-indent)
(defvar orary/disable-whitespace-cleanup nil)
(make-variable-buffer-local 'orary/disable-whitespace-cleanup)

(defvar orary/disable-clean-and-indent nil)

(defun orary/clean-and-indent-buffer ()
  "Clean up the indentation of the current buffer according to its major mode,
then clean up white space."
  (interactive)
  (unless orary/disable-clean-and-indent
    (unless (or (-contains? orary/indent-sensitive-modes major-mode)
                (-filter #'derived-mode-p orary/indent-sensitive-modes)
                orary/disable-auto-indent)
      (indent-region (point-min) (point-max)))
    (unless orary/disable-whitespace-cleanup
      (ethan-wspace-clean-all))))

;; Fun special case: tabs are required in makefiles
(defun respect-makefile-tabs ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(add-hook 'makefile-gmake-mode-hook #'respect-makefile-tabs)
(add-hook 'makefile-bsdmake-mode-hook #'respect-makefile-tabs)
(add-hook 'makefile-mode-hook #'respect-makefile-tabs)

(defun orary/programming-defaults ()
  "Set defaults for programming modes."
  (setq mode-require-final-newline nil)
  (hl-line-mode +1)
  (orary/important-comments)

  (add-hook 'before-save-hook 'orary/clean-and-indent-buffer)
  )

(add-hook 'orary/programming-mode-hook 'orary/programming-defaults)
(add-hook 'prog-mode-hook 'orary/programming-mode)

(defun orary/unhook-whitespace-cleanup (func &rest funcargs)
  "Removes from function FUNC any hooks which will alter its
whitepace, calls FUNC with FUNCARGS, then restores the hooks.
Most useful for org export."
  (interactive)
  (-let [func-res nil]
    (remove-hook 'prog-mode-hook #'orary/programming-mode)
    (remove-hook 'before-save-hook 'orary/clean-and-indent-buffer)
    (remove-hook 'before-save-hook 'ethan-wspace-clean-before-save-hook)
    (remove-hook 'after-save-hook  'ethan-wspace-clean-after-save-hook)
    (setq func-res (apply func funcargs))
    ;; (message "Whether you believe it or not, func returned %s" func-res)
    (add-hook 'prog-mode-hook 'orary/programming-mode)
    (add-hook 'before-save-hook 'orary/clean-and-indent-buffer)
    (add-hook 'before-save-hook 'ethan-wspace-clean-before-save-hook)
    (add-hook 'after-save-hook  'ethan-wspace-clean-after-save-hook)
    ;; Return out the function result
    func-res))

;; (advice-add 'org-export-to-file :around  #'orary/unhook-whitespace-cleanup)

;;---------------------------------Compilation-----------------------------------

(defvar orary/compilation-key-words-regexp
  "\\[INFO\\]\\|\\[WARN\\]")

(defvar orary/compilation-filter-regexp
  "\\[INFO\\]\\|\\[WARN\\]")

(defun orary/fontify-compilation ()
  (font-lock-add-keywords nil '(("^\\[\\(INFO\\)\\]" 1 font-lock-keyword-face t)
                                ("^\\[\\(WARN\\)\]" 1 font-lock-warning-face t)
                                ("^\\[\\(ERROR\\)\]" 1 font-lock-warning-face t))))

(defun orary/filtered-compilation ()
  (when (re-search-backward orary/compilation-filter-regexp compilation-filter-start t)
    (let ((inhibit-read-only t))
      (delete-region compilation-filter-start (point-max)))))

(defun orary/do-filtered-compilation (comp-dir comp-cmd)
  (interactive "DCompilation directory:  \nsCompilation command:  ")
  (let ((default-directory comp-dir))
    (add-hook 'compilation-filter-hook 'orary/filter-compilation)
    (compilation-start comp-cmd)
    (remove-hook 'compilation-filter-hook 'orary/filter-compilation)))

(add-hook 'compilation-filter-hook 'orary/fontify-compilation)
(setq compilation-scroll-output 'first-error)


;;---------------------------------ANSI Colors-----------------------------------
(require 'ansi-color)
(setq ansi-color-for-comint-mode t)
(defun orary/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-filter-hook 'orary/colorize-compilation-buffer)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(setq compilation-scroll-output 'first-error)

(provide 'orary-programming)
;;; orary-programming.el ends here
