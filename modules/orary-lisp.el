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
  (rainbow-mode +1)
  (rainbow-delimiters-mode-enable)
  (smartparens-strict-mode +1))

(provide 'orary-lisp)
;;; orary-lisp.el ends here
