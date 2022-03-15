;;; package --- Summary:

;;; Commentary:
;; "Use org to load config from ~/.dotfiles/emacs/configuration.org.
;; First we'll need the org package.  Use M-x install-package org if you do not
;; already have it Ensure that all config files are in the correct dir.
;; This file can then be symlinked to your home directory.  Everything after
;; ';;; init.el ends here' is auto-inserted by Emacs via customize-variable.
;; If you don't like those lazy configs just delete everything after the ends
;; here line"

;;; Code:

;; Bootstrap Quelpa so that I may use `org-tangle-config'
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(setq quelpa-update-melpa-p nil) ; Please don't attempt to replace package.el
(require 'quelpa)

;;; Use a custom variables file. Load these variables first.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))

;; Load config.el if it exists. If it does not, tangle the configuration file
;; and prompt the user to restart. By using this method, we remove any need
;; to (require 'org) so early in the configuration.
(quelpa '(org-tangle-config
          :fetcher github :repo "trev-dev/org-tangle-config.el"))
(org-tangle-config-before-load "~/.config/emacs/config.org")
(load custom-file 'noerror 'nomessage)

(provide 'emacs)
;;; init.el ends here
