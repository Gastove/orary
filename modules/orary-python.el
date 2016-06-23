;;; orary-python.el --- Python configs
;;
;;; Commentary:
;;
;;; Code:

;; Python
(use-package elpy
  :config
  (elpy-enable)
  (require 'electric)
  (elpy-use-ipython)
  (setq python-fill-docstring-style 'django)

  (defun orary/python-mode-settings ()
    (setq-local electric-layout-rules
                '((?: . (lambda ()
                          (and (zerop (first (syntax-ppss)))
                               (python-info-statement-starts-block-p)
                               'after)))))
    (add-hook 'post-self-insert-hook
              #'electric-layout-post-self-insert-function nil 'local)
    (when (fboundp #'python-imenu-create-flat-index)
      (setq-local imenu-create-index-function
                  #'python-imenu-create-flat-index))
    (setq fill-column 79)
    (define-key python-mode-map (kbd "C-c q i") 'orary/python-make-module)
    (define-key python-mode-map (kbd "C-c q r") 'orary/replace-double-quote-with-single))

  (defun orary/python-make-module ()
    (interactive)
    (let ((target (expand-file-name "__init__.py")))
      (if (file-exists-p target)
          (message "Init file already exists!")
        (progn
          (write-region "" nil target)
          (message "Init file created for module")))))


  (add-hook 'python-mode-hook #'orary/python-mode-settings))


(provide 'orary-python)
;;; orary-python.el ends here
