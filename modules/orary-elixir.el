;;; orary-elixir.el --- Elixir configs in Orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun orary/elixir-insert-pipe ()
  (interactive)
  (if (looking-back "\\[" 1)
      (sp-insert-pair "|")
    (orary/insert-key-seq "|" ">" "<" 1)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "|") #'orary/elixir-insert-pipe)
       map))))

(use-package elixir-mode
  :hook (elixir-mode . (lambda ()
                         (subword-mode +1)
                         (lsp)))
  :init (add-to-list 'exec-path "/opt/elixir/1.11/lsp")

  :bind (:map elixir-mode-map
              ("|" . orary/elixir-insert-pipe)))

(provide 'orary-elixir)
;;; orary-elixir.el ends here
