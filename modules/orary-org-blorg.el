;;; orary-org-blorg.el --- Configs as I get org-blorg together
;;
;;; Commentary:
;; `org-blorg' is my dear sweet blogging framework. It is very nice, and also
;; more... than a little WIP. So here's a tidy place for its configs to live in
;; while I... figure out... which parts of this idea are a good idea.
;;; Code:

(setq maybe-blorg-path (f-expand "Code/org-blorg/" orary/user-home-dir))

(use-package org-blorg
  :if (f-exists? maybe-blorg-path)
  :load-path maybe-blorg-path
  :mode ("\\.blorg\\'" . org-blorg-mode))

(provide 'orary-org-blorg)
;;; orary-org-blorg.el ends here
