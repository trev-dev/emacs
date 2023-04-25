;;; td-prog-mode.el --- Command function library -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file contains useful commands for binding/using inside Emacs.
;;
;;; Code:

(defun td-tree-sitter-start ()
  "Start up tree-sitter."
  (interactive)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(defun td-forward-chunk ()
  (interactive)
  (next-line 20))

(defun td-backward-chunk ()
  (interactive)
  (previous-line 20))

(provide 'td-commands)
;; td-commands.el ends here
