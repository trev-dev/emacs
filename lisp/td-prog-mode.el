;;; td-prog-mode.el --- Helper functions library -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file contains universally helpful settings for programming modes.
;;
;;; Code:

(defun td-infer-indentation-style ()
  "Figure out whether or not we are indenting with tabs or spaces.
Set `indent-tabs-mode' accordingly."
  (let ((space-count (how-many "^  "))
        (tab-count (how-many "^\t")))
    (if (> space-count tab-count)
        (setq indent-tabs-mode nil))
    (if (> tab-count space-count)
        (setq indent-tabs-mode t))))

(defun td-setup-prog-mode ()
  "A general set-up hook for `prog-mode'."
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq whitespace-display-mappings '((tab-mark 9 [9474 9] [92 9])))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#636363")))))
  (setq-local fill-column 80)
  (setq-local show-trailing-whitespace t)
  (show-paren-mode t)
  (hs-minor-mode)
  (display-line-numbers-mode)
  (display-fill-column-indicator-mode)
  (electric-pair-local-mode)
  (td-infer-indentation-style)
  (whitespace-mode))

(setq indent-tabs-mode nil
      standard-indent 2
      backward-delete-char-untabify-method 'hungry
      ediff-window-setup-function 'ediff-setup-windows-plain
      tab-width 2)

(setq-default electric-indent-inhibit t)

(add-hook 'prog-mode-hook #'td-setup-prog-mode)

(provide 'td-prog-mode)
;; td-prog-mode.el ends here
