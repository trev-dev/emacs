;;; td-denote.el --- LSP configuraton -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; My bespoke god-mode configuration.  It includes numerous features:
;;
;; - Seeking with "f" and "t", similar to vim
;; - Insert "after" the character at point. Adapts to the direction the point
;;   moved last
;; - Helpful advice
;;
;;; Code:

(setq denote-directory (expand-file-name "~/Org/denote/")
      denote-file-type 'org
      denote-known-keywords '("journal" "programming" "foss" "idea")
      denote-prompts '(title keywords)
      denote-link-fontify-backlinks t)

(defun denote/journal ()
  "Create an entry tagged 'journal', while prompting for a title."
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))

(defun denote/jump-to-denote-directory ()
  "Open dired in the `denote-directory'."
  (interactive)
  (dired denote-directory))

(defvar denote/keymap
  (let ((m (make-sparse-keymap)))
    (td-bind-keys '(("n" . denote)
                    ("J" . denote/journal)
                    ("t" . denote-template)
                    ("i" . denote-link)
                    ("I" . denote-link-add-links)
                    ("f" . denote-link-find-file)
                    ("b" . denote-link-backlinks)
                    ("r" . denote-rename-file)
                    ("R" . denote-rename-file-using-front-matter)
                    ("j" . denote/jump-to-denote-directory)
                    ("l" . denote-link))
                  m)
    m))

(fset 'denote/keymap denote/keymap)

(global-set-key (kbd "C-c n") denote/keymap)

(with-eval-after-load 'dired
    (td-bind-keys '(("C-c n i" . denote-link-dired-marked-notes)
                    ("C-c n r" . denote-dired-rename-marked-files)
                    ("C-c n R" .
                     denote-dired-rename-marked-files-using-front-matter))
                  dired-mode-map))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(add-hook 'find-file-hook #'denote-link-buttonize-buffer)
(add-hook 'dired-mode-hook #'denote-dired-mode)

(provide 'td-denote)
;;; td-denote.el ends here
