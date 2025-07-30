;;; orary-emacs-lisp.el --- Specifics for Emacs-Lisp -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'orary-lisp)
(require 'orary-functions)

(defun orary/elisp-defaults ()
  "Set defaults for Emacs Lisp buffers."
  (define-key emacs-lisp-mode-map (kbd "C-c C-u") 'orary/elisp-unbind-symbol)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'orary/lisp-defaults)
(add-hook 'emacs-lisp-mode-hook #'orary/elisp-defaults)

(provide 'orary-emacs-lisp)
;;; orary-emacs-lisp.el ends here
