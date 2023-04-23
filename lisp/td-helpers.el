;;; td-helpers.el --- Helper functions library -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file contains helper functions, mostly for setting up Emacs.
;;
;;; Code:

(defun td-init-time-from (start-time)
  "Calculate the time difference from `START-TIME' until now."
  (float-time (time-subtract (current-time) start-time)))

(defun td-report-init-time ()
  "Prints the init time into the scratch buffer as a comment."
  (switch-to-buffer "*scratch*")
  (insert-before-markers
   (format ";; Init finished in %0.2f seconds; welcome to Emacs.\n"
           (td-init-time-from emacs-startup-time))))

(defun td-bind-keys (conses &optional mode-map)
  "Bind several keybinds using a list of `CONSES'.
Binds will be global unless the optional `MODE-MAP' is specified."
  (dolist (combo conses)
    (if (or (consp mode-map) (keymapp mode-map))
        (define-key mode-map (kbd (car combo)) (cdr combo))
      (if mode-map (warn "Optional %s `MODE-MAP' was invalid: %s" (type-of mode-map) mode-map))
      (global-set-key (kbd (car combo)) (cdr combo)))))

(defun td-add-hooks (modes func)
  "Set several hooks from a list of `CONSES'.
Adds '-hook' onto the end of the symbols for brevity."
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(defun td-auto-mode (modes)
  "Add the `MODES' to the `auto-mode-alist'."
  (dolist (mode modes)
    (add-to-list 'auto-mode-alist mode)))

(provide 'td-helpers)
;;; td-helpers.el ends here
