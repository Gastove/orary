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

;; Prepare for the coming of ethan-wspace
(setq mode-require-final-newline nil)

(use-package ethan-wspace
  :commands ethan-wspace-clean-all
  :config (global-ethan-wspace-mode 1)
  :diminish ethan-wspace-mode)

(defun orary/programming-defaults ()
  "Set defaults for programming modes."
  (hl-line-mode +1)
  (orary/important-comments))

(defvar orary/indent-sensitive-modes '())

(defun orary/clean-and-indent-buffer ()
  "Clean up the indentation of the current buffer according to major mode,
then clean up white space."
  (interactive)
  (unless (-contains? orary/indent-sensitive-modes major-mode)
    (indent-region (point-min) (point-max)))
  (ethan-wspace-clean-all))

(add-hook 'orary/programming-mode-hook 'orary/important-comments)
(add-hook 'prog-mode-hook 'orary/programming-mode)

(provide 'orary-programming)
;;; orary-programming.el ends here
