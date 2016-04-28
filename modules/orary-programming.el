;;; orary-programming.el --- Defaults for Programming Language Modes
;;
;;; Commentary:
;; Tools like smartparens, flycheck, etc.
;;; Code:


;; TODO: why doesn't this work lsdkfjsldkj
(defun orary/important-comments ()
  "Some source code comments are important; make them shine.

Currently catches: FIX(ME)?:, TODO:, NOTE:."
  (font-lock-add-keywords
   nil
   '(("\\<\\(\\(TODO\\|FIX\\(ME\\)?\\NOTE\\):\\)" 1 font-lock-warning-face t))))

(defun orary/programming-defaults ()
  (orary/important-comments))

(setq orary/prog-mode-hook 'orary/important-comments)
(add-hook 'prog-mode-hook (lambda () (run-hooks 'orary/prog-mode-hook)))

(provide 'orary-programming)
;;; orary-programming.el ends here
