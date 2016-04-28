;;; orary-emacs-lisp.el --- Specifics for Emacs-Lisp
;;
;;; Commentary:
;;
;;; Code:

(require 'orary-lisp)

(defun orary/elisp-defaults ()
  (define-key lisp-mode-map (kbd "C-c C-b") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'orary/lisp-defaults)
(add-hook 'emacs-lisp-mode-hook #'orary/elisp-defaults)

(provide 'orary-emacs-lisp)
;;; orary-emacs-lisp.el ends here
