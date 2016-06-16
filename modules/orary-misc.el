;;; orary-misc.el --- Small language configs
;;
;;; Commentary:
;; Sometimes I try out a language. Sometimes I don't know if it needs its own
;; module. This is for those things.
;;; Code:

(use-package nim-mode
  :mode "\\.nim\\'"
  :config
  (require 'company-nim)
  (require 'flycheck-nim-async)
  (add-to-list 'company-backends '(company-nim :with company-nim-builtin)))

(provide 'orary-misc)
;;; orary-misc.el ends here
