;;; td-evil.el --- LSP configuraton -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; An evil configuration that suits my needs.
;;
;;; Code:
(require 'td-helpers)

;;; Keybinds
(defun td-evil-bind-keys ()
  "Create some extra evil bindings."
  (evil-set-leader 'normal (kbd "SPC"))
  ;; Avy
  (evil-define-key 'normal 'global (kbd "<leader>;") 'avy-goto-char-timer)
  ;; General
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bn") 'next-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bp") 'previous-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>x") 'execute-extended-command)

  ;; Org
  (evil-define-key 'normal org-mode-map (kbd "<leader>ci") 'org-clock-in)
  (evil-define-key 'normal org-mode-map (kbd "<leader>co") 'org-clock-out)
  (evil-define-key 'normal org-mode-map (kbd "<leader>'") 'org-edit-special)
  (evil-define-key 'normal org-src-mode-map (kbd "<leader>'") 'org-edit-special)
  (evil-define-key 'motion org-agenda-mode-map (kbd "sf") 'org-agenda-filter)

  (evil-define-key 'normal 'global (kbd "<leader>a") 'org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>cg") 'org-clock-goto)
  ;; Magit
  (evil-define-key 'normal 'global (kbd "gs") 'magit))

(with-eval-after-load 'evil
  ;; Surround
  (global-evil-surround-mode)

  ;; Goggles
  (setq evil-goggles-pulse t)
  (defun evil-goggles-start-later ()
    "Load goggles later so that it works better in a client frame."
    (evil-goggles-mode))

  (add-hook 'evil-insert-state-entry-hook #'evil-goggles-start-later)
  (add-hook 'evil-insert-state-exit-hook
            #'(lambda ()
                (remove-hook
                 'evil-insert-state-entry-hook 'evil-goggles-start-later)))

  ;; Commentary
  (add-hook 'prog-mode-hook #'evil-commentary-mode)

  ;; Evil Exchange
  (evil-exchange-install)

  ;; Matchit
  (global-evil-matchit-mode 1)

  ;; Evil Collection
  (evil-collection-init)

  ;; Evil Org
  (with-eval-after-load 'org
    (require 'evil-org)
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (add-hook 'org-mode-hook #'evil-org-mode)))

(defun evil/start ()
  "Start evil-mode."
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (require 'evil)
  (setq evil-visual-state-cursor 'hbar
        evil-insert-state-cursor '(bar . 4))
  (customize-save-variable 'evil-undo-system 'undo-redo)
  (td-evil-bind-keys)
  (evil-mode 1))

(add-hook 'after-init-hook #'evil/start)

(provide 'td-evil)
;;; td-evil.el ends here
