;;; orary-company.el --- company-mode in Orary
;;
;;; Commentary:
;;
;;; Code:

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay .4
	company-minimum-prefix-length 2
	company-tooltip-limit 20)
  :diminish company)

(provide 'orary-company)
;;; orary-company.el ends here
