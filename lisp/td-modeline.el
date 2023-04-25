;;; td-modeline.el --- A custom modeline -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Trevor Richards
;;
;; Author: Trevor Richards <trev@trevdev.ca>
;; Maintainer: Trevor Richards <trev@trevdev.ca>
;; URL: https://git.sr.ht/~trevdev/emacs.d
;; Created: 23nd April, 2023
;; Version: 0.0.1
;; package-requires: ((svg-lib "0.2.6"))
;; License: GPL3
;;
;; This file is not a part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.
;;
;; See the GNU General Public License for more details. You should have received
;; a copy of the GNU General Public License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is my own take on a fancy modeline that hopefully doesn't slow the
;; editor down.  There are a couple of concepts borrowed from "mood-line".
;;
;; Check out mood-line: https://gitlab.com/jessieh/mood-line
;;
;;; Code:
(require 'project)
(require 'td-helpers)

;;; Git
(defvar-local td-modeline-git-status-indicator ""
  "The buffer's last known workspace status.")

(put 'td-modeline-git-status-indicator 'risky-local-variable t)

(defvar td-modeline-git-status-plist
  '(unregistered ("  ⁈" . (:foreground "#C792EA"))
                 edited ("  ±" . (:foreground "#82AAFF"))
                 up-to-date ("  ✔" . success)))

(defun td-modeline-git-create-indicator (buffer)
  "Create a git status indicator using a `BUFFER'."
  (let* ((status (vc-state-refresh buffer 'git))
         (icon-and-color (plist-get td-modeline-git-status-plist status))
         (branch (cond ((eq status 'unregistered) "untracked")
                       ((or vc-mode (progn (vc-refresh-state) vc-mode))
                        (substring vc-mode 5))
                       (t ""))))
    (propertize
     (concat
      (car icon-and-color) " " branch)
     'face (cdr icon-and-color))))

(defun td-modeline-git-cache-status (&optional _frame)
  "Set local buffer's git cache status."
  (let ((proj (project-current))
        (buff (buffer-file-name)))
    (when (and proj (member buff (project-files proj)))
      (setq td-modeline-git-status-indicator
            (td-modeline-git-create-indicator buff)))))

(defun td-modeline-git-cache-status-post-magit ()
  "Iterate the project buffers and cache their magit status."
  (let ((start-buffer (current-buffer)))
    (dolist (buff (seq-filter #'td-is-file-buffer
                              (project-buffers (project-current))))
      (switch-to-buffer buff)
      (setq td-modeline-git-status-indicator (td-modeline-git-create-indicator
                                      (buffer-file-name buff))))
    (switch-to-buffer start-buffer)))

(add-hook 'after-save-hook #'td-modeline-git-cache-status)
(add-to-list 'window-buffer-change-functions #'td-modeline-git-cache-status)
(add-hook 'magit-post-refresh-hook #'td-modeline-git-cache-status-post-magit)


;;; Project.el
(defcustom td-modeline-custom-project-name nil
  "A custom directory-local name for a project.el project."
  :group 'editor
  :type 'string)

(defvar-local td-modeline-project-mode-line-name ""
  "Project the current buffer belogns to.")

(put 'td-modeline-project-mode-line-name 'risky-local-variable t)

(defun td-modeline-project ()
  "Display the current project name, or path."
  (let ((pc (project-current))
        (pname-not-set (string= td-modeline-project-mode-line-name "")))
    (if (and pc pname-not-set)
        (setq td-modeline-project-mode-line-name
              (format-mode-line
               (list
                `(:propertize ,(or td-modeline-custom-project-name
                                   (file-name-nondirectory
                                    (directory-file-name
                                     (project-root pc))))
                              face success
                              help-echo "Switch project"
                              mouse-face (:box 1)
                              local-map ,(make-mode-line-mouse-map
                                          'mouse-1 #'project-switch-project))
                '(:propertize ":" face (:inherit font-lock-comment-face)))))
      td-modeline-project-mode-line-name)))


;;; Modal editing indicators:
(defun td-modeline-modal-face (str base)
  "Propertize a `STR' with a face that inherits some `BASE' properties."
  (propertize str 'face
              `(:inherit ,base :weight bold :height 0.9)))

(defvar td-modeline-custom-meow-states `((normal . ,(td-modeline-modal-face
                                             "<N>" '(:foreground "#FFCA41")))
                                 (motion . ,(td-modeline-modal-face
                                             "<M>" '(:foreground "#82AAFF")))
                                 (keypad . ,(td-modeline-modal-face
                                             "<K>" '(:foreground "#89DDFF")))
                                 (insert . ,(td-modeline-modal-face
                                             "<I>" '(:foreground "#C792EA")))
                                 (beacon . ,(td-modeline-modal-face
                                             "<B>" '(:foreground "#FF7B85")))))

(defvar td-modeline-evil-states `((normal . ,(td-modeline-modal-face
                                      "<N>" '(:foreground "#FFCA41")))
                          (motion . ,(td-modeline-modal-face
                                      "<M>" '(:foreground "#82AAFF")))
                          (operator . ,(td-modeline-modal-face
                                        "<O>" '(:foreground "#89DDFF")))
                          (insert . ,(td-modeline-modal-face
                                      "<I>" '(:foreground "#ABDC88")))
                          (visual . ,(td-modeline-modal-face
                                       "<V>" '(:foreground "#FF996B")))
                          (replace . ,(td-modeline-modal-face
                                       "<R>" '(:foreground "#FF7B85")))
                          (emacs . ,(td-modeline-modal-face
                                     "<E>" '(:foreground "#C792EA")))))

(defun td-modeline-meow-state ()
  "Retrieve the meow-state for the mode-line."
  (when (fboundp 'meow--current-state)
    (concat (alist-get (meow--current-state) td-modeline-custom-meow-states)
            " ")))

(defun td-modeline-evil-state ()
  "Get the evil state for the mode-line."
  (when (boundp 'evil-state)
    (concat (alist-get evil-state td-modeline-evil-states)
            " ")))


;;; God Mode:
(defvar td-modeline-god-mode-icon
  (propertize "<G>" 'face '(:foreground "#FFCA41" :weight bold))
  "The hammer of the gods, but only if you are worthy.")

(defvar-local td-modeline-god-mode-indicator--cached ""
  "The cached state of the `god-local-mode' indicator for the mode-line.")

(put 'td-modeline-god-mode-indicator--cached 'risky-local-variable t)

(defun td-modeline-god-mode-icon--make-icon ()
  "Produce the hammer of the gods."
  (propertize
   "  " 'display (when (functionp 'svg-lib-icon)
                   (svg-lib-icon
                    "mjolnir"
                    `(:collection "local"
                      :stroke 0
                      :padding 0
                      :width 20
                      :foreground ,(face-foreground 'warning)
                      :background ,(face-background 'mode-line))))))

(defun td-modeline-god-mode-icon--set-icon ()
  "Set the `td-modeline-god-mode-icon' as an svg icon in graphical displays."
  (when (display-graphic-p)
    (setq td-modeline-god-mode-icon (td-modeline-god-mode-icon--make-icon))))

(defun td-modeline-god-mode-setup-icon ()
  "Set up the `td-modeline-god-mode-icon' as late as possible in the init process.

The reasoning for this is because if the window's not ready to go, an SVG
icon renders incorrectly."
  (td-modeline-god-mode-icon--set-icon)
  (td-modeline-god-mode-indicator--update-cached))

(defun td-modeline-god-mode-indicator--update-cached ()
  "A hook function for setting `td-modeline-god-mode-indicator'."
  (setq td-modeline-god-mode-indicator--cached
        (format
         "%s" (if (bound-and-true-p god-local-mode)
                  td-modeline-god-mode-icon
                ""))))

(defun td-modeline-god-mode-indicator ()
  "Display `td-god-mode-indicator--cached' in the mode-line."
  td-modeline-god-mode-indicator--cached)

(with-eval-after-load 'god-mode
  (defvar svg-lib-icon-collections) ; Satisfy compiler
  (require 'svg-lib)

  (add-to-list 'svg-lib-icon-collections
               (cons "local" "file:///home/trev/.emacs.d/%s.svg"))
  (td-modeline-god-mode-setup-icon)
  (add-hook 'god-local-mode-hook
            #'td-modeline-god-mode-indicator--update-cached))

(defun td-modeline-status-flag (enable face)
  "`ENABLE' a status indicator and give it a `FACE'."
  (format "%s" (if enable
                   (propertize "▰" 'face `(:inherit ,face :weight bold))
                 "-")))


;;; Location
(defvar-local td-modeline-line-number-indicator nil
  "Display buffer position in the mode-line.")

(defun td-modeline-line-number-indicator--update ()
  "Display the mode-line buffer position."
  (setq td-modeline-line-number-indicator
        (if line-number-mode (list "  %l:%c") "")))

(setq-local td-modeline-line-number-indicator
  (td-modeline-line-number-indicator--update))

(add-hook 'line-number-mode-hook #'td-modeline-line-number-indicator--update)


;;; Buffer Size
(defvar-local td-modeline-buffer-size nil
  "Display the buffer size in the mode-line.")

(defun td-modeline-buffer-size--update ()
  "Update the `td-modeline-buffer-size' mode-line variable."
  (setq td-modeline-buffer-size
        (if size-indication-mode
            '(:propertize " (%I)" :face '(:inherit font-lock-comment-face))
          "")))

(setq-local td-modeline-buffer-size (td-modeline-buffer-size--update))

(add-hook 'size-indication-mode-hook #'td-modeline-buffer-size--update)


;;; Flymake/Flycheck
(defun td-modeline-flycheck ()
  "Get the flycheck status for the buffer, if LSP mode is not doing so."
  (when (and (fboundp 'flycheck-count-errors)
             (boundp 'flycheck-current-errors)
             (not (bound-and-true-p lsp-mode)))
    (let* ((errlist (flycheck-count-errors flycheck-current-errors))
           (warnings (alist-get 'warning errlist))
           (errors (alist-get 'error errlist)))
      (concat
       (when warnings
         (propertize (format "  %s%s"
                             warnings (if errors "/" ""))
                     'face 'warning))
       (when errors
         (propertize (format
                      "%s%s" (if warnings "" "  ") errors)
                     'face 'error))))))

(defun td-modeline-flymake ()
  "Display the flymake status for the buffer."
  (when (boundp 'flymake-mode-line-counters) " "
    (list "  " flymake-mode-line-counters)))


;;; Misc
(defun td-modeline-misc ()
  "Get a trimmed version of the `mode-line-misc-info'."
  (let ((info (format-mode-line mode-line-misc-info)))
    (unless (string= info "")
      (list "  " (string-trim info)))))

(defun td-modeline-macro-indicator ()
  "Indicate when a macro is being recorded in the mode-line."
  (when defining-kbd-macro
    (format "%s" (propertize
                  "λ" 'face '(:inherit bold :foreground "#C792EA")))))

(defun td-modeline-anzu-indicator ()
  "An Anzu count indicator."
  (when (fboundp 'anzu--update-mode-line)
    (concat " " (anzu--update-mode-line))))


;;; Formatter
(defun td-modeline-split-format (left right)
  "Format a mode-line with a `LEFT' and `RIGHT' justified list of elements.
The modeline should fit the `window-width' with space between the lists."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display
                        `((space :align-to
                                 (- right (- 0 right-margin) ,reserve))))
            right)))

(setq-default mode-line-format
              '((:eval
                 (td-modeline-split-format
                  ;; Left
                  (format-mode-line
                   '(" "
                     (:eval (td-modeline-meow-state))
                     (:eval (td-modeline-evil-state))
                     (:eval (td-modeline-status-flag buffer-read-only 'error))
                     (:eval (td-modeline-status-flag (buffer-modified-p)
                                                     'warning))
                     (:eval (if (not (eq (format-mode-line mode-line-client) ""))
                                (td-modeline-status-flag t '(:foreground "#C792EA")) ""))
                     " "
                     (:eval (td-modeline-project))
                     mode-line-buffer-identification
                     td-modeline-line-number-indicator
                     (:eval (td-modeline-anzu-indicator))
                     td-modeline-buffer-size
                     (:propertize " %p%%" face (:inherit font-lock-comment-face))))
                  ;; Right
                  (format-mode-line
                   '((:eval (td-modeline-god-mode-indicator))
                     (:eval (td-modeline-macro-indicator))
                     td-modeline-git-status-indicator
                     (:eval (td-modeline-flycheck))
                     (:eval (td-modeline-misc))
                     "  "
                     mode-line-modes))))))

(provide 'td-modeline)
;;; td-modeline.el ends here
