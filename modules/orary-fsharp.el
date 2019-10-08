;;; orary-fsharp.el --- F# language configs for orary
;;
;;; Commentary:
;; F# mode configuration, smartparens pairs.
;;; Code:

(require 'smartparens)
(require 'dash)
(require 'dabbrev)

(defun orary/sp-insert-pipe-p (delim action ctxt)
  (save-excursion
    (and (eq ctxt 'code)
         (not (bolp))
         (looking-back "[(\\[]|" 2))))

(defun orary/sp-insert-at-p (delim action ctxt)
  (save-excursion
    (and (eq ctxt 'code)
         (not (bolp))
         (looking-back "<\\|<@\\{1,2\\}" 2))))

(defun orary/sp-close-open-angle-p (delim action ctxt)
  (save-excursion
    (looking-back "[^|-]" 2)))

(defun orary/sp-correct-angle-bracket (delim action ctxt)
  (save-excursion
    (when (looking-at-p ">@")
      (delete-char 2)
      (insert "@>"))))

(defun fsharp-ret-dwim ()
  "Look for an operator in the currently line -- one of |, |>,
||>, |||>. Insert a newline after current line, indent, and
insert the found character, along with a proper yasnippet for
types and match blocks. Note: this function currently does not
support classes, even though it would be consistent from a
usability standpoint to do so."
  (interactive)
  (let ((search-bol (line-beginning-position))
        (search-eol (line-end-position))
        (type-yas-tpl "$1${2: of $0}")
        (match-yas-tpl "$1 -> $0")
        insertion-char
        insertion-tpl)
    (beginning-of-line)
    (cond
     ;; Discriminated union types
     ((looking-at "\\s-*type\\|\\s-*| [A-Za-z'_]+\\(?: of.*\\)?$")
      (setq insertion-char "|"
            insertion-tpl type-yas-tpl))
     ;; Match blocks
     ((looking-at "\\s-*match .* with\\|\\s-*| .*->")
      (setq insertion-char "|"
            insertion-tpl match-yas-tpl))
     ;; Arbitrary piping expressions
     ((looking-at "\\s-*\\(|\\{1,3\\}>\\)")
      (setq insertion-char (match-string 1))))
    (end-of-line)
    (fsharp-newline-and-indent)
    (when insertion-char
      (insert insertion-char)
      (insert " ")
      (when insertion-tpl
        (yas-expand-snippet insertion-tpl)))))

(defun orary/make-fantomas-tmp-buffer ()
  (-let [buf (get-buffer-create "*Fantomas Output*")]
    (with-current-buffer buf
      (fsharp-mode)
      (read-only-mode))
    buf))

(defun orary/fsharp-format-buffer (arg)
  "Formats the current buffer with `fantomas' if installed; see
  https://github.com/fsprojects/fantomas for details. With prefix
  argument ARG, shows a preview of the output instead of
  overwriting the current buffer."
  (interactive "P")
  (-if-let (fantomas-command (executable-find "fantomas"))
      (-let [target-buffer (if arg
                               (orary/make-fantomas-tmp-buffer)
                             (buffer-name))]
        (shell-command-on-region
         (point-min)
         (point-max)
         (s-join " "
                 (list
                  "fantomas --stdin --stdout"
                  "--pageWidth" (number-to-string fill-column)
                  "--indent" (number-to-string tab-width)))
         target-buffer))
    (message "Couldn't find `fantomas' on your path"))
  )

(defun orary/fsharp-insert-pipe (multiplier)
  (interactive "p")
  (orary/fsharp-insert-key-seq "|" ">" "<" multiplier)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "|") #'orary/fsharp-insert-pipe)
     map)))

(defun orary/fsharp-insert-arrow ()
  (interactive)
  (orary/fsharp-insert-key-seq "-" ">" "<")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "-") #'orary/fsharp-insert-arrow)
     map)))

;; TODO: to make this work, I need to set a local variable -- something that
;; outlives each given call to the repeating function.
(defun orary/fsharp-insert-key-seq (base mod-one mod-two &optional multiplier)
  (-let* ((multi (case nil
                   (1 1)
                   (4 2)
                   (otherwise 1)))
          (base (s-repeat multi base))
          (right (s-concat base mod-one))
          (left (s-concat mod-two base)))
    (message "Multi is %s, right is %s, left is %s" multi right left)
    (cond ((re-search-backward right (- (point) (length right)) t)
           (replace-match left))
          ((re-search-backward left (- (point) (length left)) t)
           (replace-match base))
          ((thing-at-point-looking-at base 1) (insert mod-one))
          (:else (insert base)))))

(defun orary/lsp-fsharp-type-at ()
  (interactive)
  (lsp-request-async "fsharp/signature"
                     (lsp--text-document-position-params)
                     (lambda (response)
                       (eldoc-message (fsharp-fontify-string (cdr (assq 'Data (json-read-from-string (ht-get response "content")))))))))

(setq lsp-eldoc-hook 'orary/lsp-fsharp-type-at)

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$"
  :config
  (setq ;; company-auto-complete nil
   require-final-newline nil
   fsharp-ac-intellisense-enabled nil
   inferior-fsharp-program "dotnet fsi --readline- --noframework")

  (add-hook 'fsharp-mode-hook
            (lambda ()
              (subword-mode +1)
              (highlight-indentation-mode +1)
              (lsp-mode +1)
              (lsp)
              ))
  (sp-with-modes 'fsharp-mode
    (sp-local-pair "<" ">"
                   :when '((orary/sp-close-open-angle-p))
                   :post-handlers '(orary/sp-correct-angle-bracket))
    (sp-local-pair "@" "@"
                   :actions '(insert wrap navigate)
                   :when '(orary/sp-insert-at-p))
    (sp-local-pair "|" "|" :when '(orary/sp-insert-pipe-p))
    (sp-local-pair "\"\"\"" "\"\"\"")
    (sp-local-pair "`" "`" :actions :rem)
    (sp-local-pair "``" "``"))
  (add-hook 'fsharp-mode-hook (lambda () (setq dabbrev-check-all-buffers nil)))
  :bind (:map fsharp-mode-map
              ("<C-return>" . fsharp-ret-dwim)
              ("|" . orary/fsharp-insert-pipe)
              ("-" . orary/fsharp-insert-arrow)))

(provide 'orary-fsharp)
;;; orary-fsharp.el ends here
