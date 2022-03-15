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

;; Load config.el if it exists. If it does not, tangle the configuration file
;; and prompt the user to restart. By using this method, we remove any need
;; to (require 'org) so early in the configuration.
(add-to-list 'load-path "~/Projects/org-tangle-config")
(require 'org-tangle-config)
(org-tangle-config-before-load "~/.config/emacs/config.org")

;;; Use a custom variables file. Load these variables first.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(provide 'emacs)
;;; init.el ends here
