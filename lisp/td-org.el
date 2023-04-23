;;; td-org.el --- Org-mode configureation -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file contains my org mode configuration
;;
;;; Code:

(defvar td-org-tag-list
  '((:startgroup)
    ("@home" . ?H)
    ("@work" . ?W)
    (:endgroup)
    ("foss"  . ?f)
    ("gurps" . ?g)
    ("idea"  . ?i))
  "The tags for org headlines.")

(defvar td-org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "LOW(l)"
              "|" "DONE(d!)" "PASS(p@)" "CANC(k@)"))
  "A sequence of keywords for Org headlines.")

(defvar td-org-agenda-commands
  '(("d" "Dashboard: Get things done!"
     ((agenda "" ((org-agenda-span 7)))
      (tags-todo "+refile"
                 ((org-agenda-overriding-header "Unfiled")))
      (tags-todo "+PRIORITY=\"A\""
                 ((org-agenda-overriding-header "High Priority")
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'todo '("WAIT")))))
      (todo "NEXT"
            ((org-agenda-overriding-header "Do Next")
             (org-agenda-max-todos nil)))
      (todo "WAIT"
            ((org-agenda-overriding-header "Follow Up")))
      (todo "TODO"
            ((org-agenda-overriding-header "Other Actionables")
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
    ("l" "Backburner of low priority tasks"
     ((todo "LOW"
           ((org-agenda-overriding-header "Someday/Maybe"))))))
  "Custom commands for Org Agenda.")

(defvar td-org-capture-templates
  '(("t" "Todo" entry (file "~/Org/agenda/inbox.org")
     "* TODO %^{Title: }\n:PROPERTIES:\n:date: %U\n:END:\n%?"
     :empty-lines 1)
    ("c" "Contact" entry (file+headline "~/Org/contacts.org" "Other")
     "* %^{Name: }\n:PROPERTIES:\n:email: %?\n:END:"
     :empty-lines 1))
  "Base org-capture-templates.")

(defun td-org-append-templates (templates)
  "Append `TEMPLATES' to `org-capture-templates'."
  (setq org-capture-templates (append org-capture-templates templates)))

(add-hook 'org-mode-hook
          #'(lambda ()
              "Do some stuff on org mode startup."
              (org-clock-persistence-insinuate)
              (org-indent-mode)
              (setq-local line-spacing 0.1)))

(global-set-key (kbd "C-c a") #'org-agenda)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e t") #'org-table-export))

(with-eval-after-load 'ox
  (require 'ox-md nil t))

(setq org-fontify-quote-and-verse-blocks t
      org-attach-auto-tag "attach"
      org-directory "~/Org"
      org-archive-location "archives/%s_archive::"
      org-log-done 'time
      org-log-into-drawer t
      org-enforce-todo-dependencies t
      org-src-preserve-indentation t
      org-clock-persist 'history
      org-agenda-block-separator "──────────"
      org-agenda-tags-column -80
      org-duration-format '(("h" . nil) (special . 2))
      org-clock-total-time-cell-format "%s"
      org-agenda-files '("~/Org/agenda")
      org-tag-alist td-org-tag-list
      org-todo-keywords td-org-todo-keywords
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-outline-path-complete-in-steps t
      org-refile-targets '((org-agenda-files :maxlevel . 4)
                           ("contacts.org" :maxlevel . 1))
      org-clock-sound "~/.config/emacs/inspectorj_bell.wav"
      org-timer-default-timer "25"
      org-agenda-custom-commands td-org-agenda-commands
      org-stuck-projects '("/PROJ-DONE" ("TODO" "NEXT") nil "- \\[ \\]")
      org-capture-templates td-org-capture-templates
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t)

;; Fix completion in steps for org-refile
(advice-add #'org-olpath-completing-read :around
            (lambda (&rest args)
              (minibuffer-with-setup-hook
                  (lambda () (setq-local completion-styles '(basic)))
                (apply args))))

;;; Exporters
(with-eval-after-load 'ox
  (require 'ox-gfm)
  (require 'ox-hugo))

;;; Presenter
(setq org-present-text-scale 5)
(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook
            #'(lambda ()
                (org-present-big)
                (td/org-scale-levels-toggle t)
                (org-display-inline-images)
                (blink-cursor-mode -1)
                (org-present-hide-cursor)
                (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            #'(lambda()
                (org-present-small)
                (org-remove-inline-images)
                (org-present-show-cursor)
                (blink-cursor-mode 1)
                (td/org-scale-levels-toggle)
                (org-present-read-write)))
  (td/bind-keys '(("C-c C-p C-c" . org-present-show-cursor)
                  ("C-c C-p C-h" . org-present-hide-cursor))
                org-present-mode-keymap))

;;; Org View
(with-eval-after-load 'org
  (require 'org-view-mode)
  (setq org-view-font-enable t)
  (add-to-list 'org-view-font-remaps '(default . (:family "Carlito"))))

(provide 'td-org)
;;; td-org.el ends here
