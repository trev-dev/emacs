;;; package --- Summary:

;;; Commentary:
;; "Use org to load config from ~/.dotfiles/emacs/config.org.
;; First we'll need the org package.  Use M-x install-package org if you do not
;; already have it Ensure that all config files are in the correct dir.
;; This file can then be symlinked to your home directory.  Everything after
;; ';;; init.el ends here' is auto-inserted by Emacs via customize-variable.
;; If you don't like those lazy configs just delete everything after the ends
;; here line"

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package org :straight (:type built-in))


;;; Use a custom variables file. Load these variables first.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))

;; Load config.el if it exists. If it does not, tangle the configuration file
;; and prompt the user to restart. By using this method, we remove any need
;; to (require 'org) so early in the configuration.n

(org-babel-load-file "~/.config/emacs/config.org")

(load custom-file 'noerror 'nomessage)

(provide 'emacs)
;;; init.el ends here
