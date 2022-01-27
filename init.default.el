;;; package --- Summary:

;;; Commentary:
;; "Use org to load config from ~/.dotfiles/emacs/configuration.org.
;; First we'll need the org package.  Use M-x install-package org if you do not
;; already have it Ensure that all config files are in the correct dir.
;; This file can then be symlinked to your home directory.  Everything after
;; ';;; init.el ends here' is auto-inserted by Emacs via customize-variable.
;; If you don't like those lazy configs just delete everything after the ends
;; here line"


;;; Packages:
(require 'org)

"Load org config"
;;; Code:
(add-to-list 'load-path "~/.config/emacs/packages")
(org-babel-load-file "~/.config/emacs/config.org")

(provide 'emacs)
;;; init.el ends here
