;;; orary-python.el --- Python configs
;;
;;; Commentary:
;;
;;; Code:

;; Python

;; (require 'python-environment)

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
              ("M-."     . ggtags-find-tag-dwim)
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

;; This is fucking preposterous, but here we are: re-defining this
;; function to make indentation not suck. - RMD 2017-09-15
(eval-after-load "python"
  (defun python-indent--calculate-indentation ()
    "Internal implementation of `python-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
    (save-restriction
      (widen)
      (save-excursion
        (pcase (python-indent-context)
          (`(:no-indent . ,_) 0) ; usually 0
          (`(,(or :after-line
                  :after-comment
                  :inside-string
                  :after-backslash
                  :inside-paren-at-closing-paren
                  :inside-paren-at-closing-nested-paren) . ,start)
           ;; Copy previous indentation.
           (goto-char start)
           (current-indentation))
          (`(:inside-docstring . ,start)
           (let* ((line-indentation (current-indentation))
                  (base-indent (progn
                                 (goto-char start)
                                 (current-indentation))))
             (max line-indentation base-indent)))
          (`(,(or :after-block-start
                  :after-backslash-first-line
                  :after-backslash-assignment-continuation
                  :inside-paren-newline-start) . ,start)
           ;; Add one indentation level.
           (goto-char start)
           (+ (current-indentation) python-indent-offset))
          (`(,(or :inside-paren
                  :after-backslash-block-continuation
                  :after-backslash-dotted-continuation) . ,start)
           ;; Use the column given by the context.
           (goto-char start)
           (current-column))
          (`(:after-block-end . ,start)
           ;; Subtract one indentation level.
           (goto-char start)
           (- (current-indentation) python-indent-offset))
          (`(:at-dedenter-block-start . ,_)
           ;; List all possible indentation levels from opening blocks.
           (let ((opening-block-start-points
                  (python-info-dedenter-opening-block-positions)))
             (if (not opening-block-start-points)
                 (prog-first-column) ; if not found default to first column
               (mapcar (lambda (pos)
                         (save-excursion
                           (goto-char pos)
                           (current-indentation)))
                       opening-block-start-points))))
          (`(,(or :inside-paren-newline-start-from-block) . ,start)
           (goto-char start)
           ;; All this to make this line not double-indent
           (+ (current-indentation) python-indent-offset)))))))

(provide 'orary-python)
;;; orary-python.el ends here
