;;; orary-work.el ---
;;
;;; Commentary:
;;
;;; Code:

;; Local work configs

(defun orary/append-org-capture-templates (templates)
  (setq org-capture-templates (append (symbol-value 'org-capture-templates) templates)))

(let ((work-configs (expand-file-name ".work.el" (getenv "HOME"))))
  (when (file-exists-p work-configs)
    (load-file work-configs)))

(provide 'orary-work)
;;; orary-work.el ends here
