;;; td-prog-mode.el --- Command function library -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file contains useful commands for binding/using inside Emacs.
;;
;;; Code:

(defun td-forward-chunk ()
  "Move forward 20 lines."
  (interactive)
  (next-line 20))

(defun td-backward-chunk ()
  "Move backward 20 lines."
  (interactive)
  (previous-line 20))

(defun wsl-copy (start end)
  "Copy region `START' to `END' and put it in the Windows clipboard."
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

(defun wsl-paste ()
  "Remove CLRF from Windows clipboard and paste it into the buffer."
  (interactive)
  (let ((clipboard
         (shell-command-to-string
          "powershell.exe -c Get-Clipboard 2> /dev/null")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
    (setq clipboard (substring clipboard 0 -1))
    (insert clipboard)))

(when (string-match-p "Linux.*WSL2.*Linux" (shell-command-to-string "uname -a"))
  (global-set-key (kbd "C-c w c") #'wsl-copy)
  (global-set-key (kbd "C-c w p") #'wsl-paste))

(provide 'td-commands)
;;; td-commands.el ends here
