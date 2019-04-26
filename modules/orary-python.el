;;; orary-python.el --- Python configs
;;
;;; Commentary:
;;
;;; Code:

;; Python

;; (require 'python-environment)

(use-package pip-requirements)

(use-package elpy
  :demand t
  :config

  (elpy-enable)
  (require 'electric)
  ;; (elpy-use-ipython)
  (setq
   ;; python-shell-interpreter-args "--simple-prompt -i"
   python-fill-docstring-style 'django
   python-environment-directory "~/.python_virtualenvs/"
   ;; There is some indication this might help with the vague weirds I've seen.
   ;; https://github.com/jorgenschaefer/elpy/issues/985#issuecomment-249113499
   ;; python-shell-completion-native-enable nil
   elpy-rpc-python-command "python3")

  (defun orary/python-mode-settings ()
    (subword-mode +1)
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
              ("C-c q r" . recompile)
              ("C-c n"   . elpy-black-fix-code )
              ("<M-RET>" . orary/rotate)))

;; Jupyter Notebooks
(require 'company)
(use-package jedi)

(use-package ein
  :demand t
  :config
  (defun ein-python-configs ()
    (jedi:setup)
    (add-to-list 'company-backends 'ein:company-backend))

  (add-hook 'ein:notebook-python-mode-hook #'orary/programming-defaults)
  (add-hook 'ein:notebook-python-mode-hook #'ein-python-configs))

(provide 'orary-python)
;;; orary-python.el ends here
