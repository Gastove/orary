;;; orary-programming.el --- Defaults for Programming Language Modes
;;
;;; Commentary:
;; Tools like smartparens, flycheck, etc.
;;; Code:

(require 'dash)

(use-package rainbow-delimiters)

(define-minor-mode orary/programming-mode "Programming behaviors for orary")

(defun orary/important-comments ()
  "Some source code comments are important; make them shine.

Currently catches: FIX(ME)?, TODO, NOTE."
  (font-lock-add-keywords
   nil
   '(("\\<\\(\\(TODO\\|FIX\\(ME\\)?\\|NOTE\\)\\)" 1 font-lock-warning-face t))))

(setq mode-require-final-newline nil)

(use-package ethan-wspace
  :commands ethan-wspace-clean-all
  :diminish ethan-wspace-mode)

(use-package ggtags)

(defvar orary/gg-tags-modes
  '(c-mode 'c++-mode java-mode emacs-lisp-mode))

(defvar orary/indent-sensitive-modes
  '(conf-mode yaml-mode scala-mode purescript-mode org-mode makefile-gmake-mode))

(defvar orary/disable-auto-indent nil)
(make-variable-buffer-local 'orary/disable-auto-indent)

(defun orary/clean-and-indent-buffer ()
  "Clean up the indentation of the current buffer according to its major mode,
then clean up white space."
  (interactive)
  (unless (or (-filter #'derived-mode-p orary/indent-sensitive-modes)
              orary/disable-auto-indent)
    (indent-region (point-min) (point-max)))
  (ethan-wspace-clean-all))

;; Fun special case: tabs are required in makefiles
(defun respect-makefile-tabs ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
(add-hook 'makefile-gmake-mode-hook #'respect-makefile-tabs)

(defun orary/programming-defaults ()
  "Set defaults for programming modes."
  ;; Prepare for the coming of ethan-wspace
  (setq mode-require-final-newline nil)
  (hl-line-mode +1)
  (orary/important-comments)
  (when (apply 'derived-mode-p orary/gg-tags-modes)
    (ggtags-mode +1))
  (add-hook 'before-save-hook 'orary/clean-and-indent-buffer))

(add-hook 'orary/programming-mode-hook 'orary/programming-defaults)
(add-hook 'prog-mode-hook 'orary/programming-mode)

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

;;---------------------------------ANSI Colors-----------------------------------
(require 'ansi-color)
(defun orary/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-filter-hook 'orary/colorize-compilation-buffer)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(setq compilation-scroll-output 'first-error)

(provide 'orary-programming)
;;; orary-programming.el ends here
