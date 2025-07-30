;;; orary-linux.el --- Linux configs for orary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'exec-path-from-shell)
(message "We're at least correctly invoking Linux configs")

(-let* ((extra (list "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LSP_USE_PLISTS"))
        (exec-path-from-shell-variables (append exec-path-from-shell-variables extra)))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK"
                                  "SSH_AGENT_PID"
                                  "GPG_AGENT_INFO"
                                  "WORKON_HOME"
                                  "GOPATH"                                  
                                  "LSP_USE_PLISTS"))

(provide 'orary-linux)
;;; orary-linux.el ends here
