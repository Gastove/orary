;;; orary-lisp.el --- Lisp Configs
;;
;;; Commentary:
;; Emacs Lisp, Clojure, Et Al
;;; Code:

;;; Languages
;; Lisp

(require 'diminish)

(defun orary/lisp-defaults ()
  "Set default values for lisp-language major modes."
  (eldoc-mode +1)
  (subword-mode +1)
  (diminish 'subword-mode)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1)
  (push '("lambda" . ?λ) prettify-symbols-alist))

(defun orary/interactive-lisp-defaults ()
  (subword-mode +1)
  (diminish 'subword-mode)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1))

(provide 'orary-lisp)
;;; orary-lisp.el ends here
