;;; orary-lisp.el --- Lisp Configs
;;
;;; Commentary:
;; Emacs Lisp, Clojure, Et Al
;;; Code:

;;; Languages
;; Lisp
(defun orary/lisp-defaults ()
  "Set default values for lisp-language major modes."
  (eldoc-mode +1)
  (subword-mode +1)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1))

(defun orary/interactive-lisp-defaults ()
  (subword-mode +1)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1))

(provide 'orary-lisp)
;;; orary-lisp.el ends here
