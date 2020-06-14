;;; orary-lisp.el --- Lisp Configs
;;
;;; Commentary:
;; Configurations common across Common Lisp, Emacs Lisp, Clojure, et al.
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

;; Scheme
(use-package geiser
  :config
  (add-hook 'scheme-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'scheme-mode-hook #'orary/lisp-defaults)
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename
        (expand-file-name "geiser-history" orary/save-root)))

(provide 'orary-lisp)
;;; orary-lisp.el ends here
