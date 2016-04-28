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
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; Correctly fill python docstrings
(setq python-fill-docstring-style 'django)

(defun orary/python-mode-settings ()
  (setq-local electric-layout-rules
	      '((?: . (lambda ()
			(and (zerop (first (syntax-ppss)))
			     (python-info-statement-starts-block-p)
			     'after)))))
  (add-hook 'post-self-insert-hook
	    #'electric-layout-post-self-insert-function nil 'local)
  (define-key 'python-mode-map (kbd "C-c q i") 'orary/python-make-module))

(defun orary/python-make-module ()
  (interactive)
  (let ((target (expand-file-name "__init__.py")))
    (if (file-exists-p target)
	(message "Init file already exists!")
      (progn
	(write-region "" nil target)
	(message "Init file created for module")))))

(add-hook 'python-mode-hook #'orary/python-mode-settings)


(provide 'orary-python)
;;; orary-python.el ends here
