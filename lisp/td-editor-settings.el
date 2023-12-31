;;; td-editor-settings.el --- General editor settings  -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file is used to initialize some pretty basic editor settings.
;;
;;; Code:

(require 'td-helpers)

;;; Window and basic functionality

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)
(setq inhibit-startup-screen t
      initial-scratch-message ""
      history-length 25
      sentence-end-double-space nil
      frame-resize-pixelwise t
      visible-bell t
      enable-local-variables t)
(global-visual-line-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(global-so-long-mode 1)

;; Autosaves and backups
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Custom vars stored elsewhere
(setq custom-file (locate-user-emacs-file "custom-vars.el"))

;; Permit more memory use to avoid too much garbage collection
(setq read-process-output-max (* 1024 1024))

;;; Spelling
(setq ispell-personal-dictionary "~/.emacs.d/personal-dict.pwd"
      ispell-dictionary "en"
      ispell-program-name "aspell"
      ispell-alternate-dictionary (concat (getenv "HOME") "/Documents/wordlist"))

(define-key text-mode-map (kbd "C-c f") #'flyspell-mode)

;;; Font
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font 13"))

;;; Generic bindings
(td-bind-keys '(("M-j" . join-line)
                ("M-n" . scroll-up-line)
                ("M-p" . scroll-down-line)))

(provide 'td-editor-settings)
;;; td-editor-settings.el ends here
