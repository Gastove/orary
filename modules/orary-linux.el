;;; orary-linux.el --- Linux configs for orary
;;
;;; Commentary:
;;
;;; Code:

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK"
                                    "SSH_AGENT_PID"
                                    "GPG_AGENT_INFO")))

(provide 'orary-linux)
;;; orary-linux.el ends here
