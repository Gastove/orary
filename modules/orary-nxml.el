;;; orary-nxml.el ---
;;
;;; Commentary:
;;
;;; Code:

;; nXML
(push 'nxml-mode sp-ignore-modes-list)
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (flyspell-mode-off)
	    (define-key prelude-mode-map (kbd "C-c C-i") 'nxml-balanced-close-start-tag-inline)))

(provide 'orary-nxml)
;;; orary-nxml.el ends here
