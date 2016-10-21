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
  ;; IPython 5 brings in a whole slew of horrible changes that make it kinda
  ;; worthless with Emacs now. Great? Great.
  ;; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5
  ;; ...well fine okay lets try it
  ;; 2016-09-18 -- Ugh, fucking nope. Keep an eye on this, in case ipython improves.
  ;; (elpy-use-ipython)

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
    (setq fill-column 79))

  (defun orary/python-make-module ()
    (interactive)
    (let ((target (expand-file-name "__init__.py")))
      (if (file-exists-p target)
          (message "Init file already exists!")
        (progn
          (write-region "" nil target)
          (message "Init file created for module")))))


  (add-hook 'python-mode-hook #'orary/python-mode-settings)
  :bind (:map python-mode-map
              ("C-c q i" . orary/python-make-module)
              ("C-c q q" . orary/replace-double-quote-with-single)
              ( "C-c q r" . recompile)
              ("M-." . ggtags-find-tag-dwim)))


(provide 'orary-python)
;;; orary-python.el ends here
