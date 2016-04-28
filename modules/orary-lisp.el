;;; orary-lisp.el --- Lisp Configs
;;
;;; Commentary:
;; Emacs Lisp, Clojure, Et Al
;;; Code:

;;; Languages
;; Lisp
(defun orary/elisp-defaults ()
  (eldoc-mode +1)
  (rainbow-mode +1)
  (rainbow-delimiters-mode-enable)
  (smartparens-strict-mode +1)
  (define-key lisp-mode-map (kbd "C-c C-b") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'orary/elisp-defaults)

(provide 'orary-lisp)
;;; orary-lisp.el ends here
