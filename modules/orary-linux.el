;;; orary-linux.el --- Linux configs for orary
;;
;;; Commentary:
;;
;;; Code:

(exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK"
                                  "SSH_AGENT_PID"
                                  "GPG_AGENT_INFO"
                                  "WORKON_HOME"
                                  "GOPATH"
                                  "LSP_USE_PLISTS"))

(provide 'orary-linux)
;;; orary-linux.el ends here
