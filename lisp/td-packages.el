;;; td-packages.el --- Package management -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file contains my package manifest and installation plan. It currently
;; uses package.el to help maintain maximum portability.
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar td-package-manifest
  '(all-the-icons
    all-the-icons-dired
    anzu
    avy
    cider
    clojure-mode
    company
    consult
    dap-mode
    denote
    diff-hl
    diminish
    direnv
    emmet-mode
    eglot
    eglot-java
    eshell-syntax-highlighting
    exec-path-from-shell
    expand-region
    flymake
    god-mode
    geiser-guile
    goggles
    highlight-indent-guides
    ledger-mode
    ligature
    magit
    marginalia
    markdown-mode
    modus-themes
    multiple-cursors
    nim-mode
    notmuch
    ol-notmuch
    orderless
    org-chef
    org-contacts
    org-mime
    org-present
    org-view-mode
    ox-gfm
    ox-hugo
    paredit
    password-store
    pinentry
    rainbow-delimiters
    rainbow-mode
    rg
    tangonov-theme
    transpose-mark
    tree-sitter
    tree-sitter-langs
    typescript-mode
    svg-lib ; td-modeline.el
    vertico
    web-mode
    which-key
    yaml-mode
    yasnippet
    yasnippet-snippets))

(dolist (pkg td-package-manifest)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun td-packages-cleanup (manifest)
  "Ensure package `MANIFEST' is set as `package-selectect-packages'.
That way, any old packages can be easily autoremoved."
  (customize-save-variable 'package-selected-packages manifest))

(add-hook 'after-init-hook
          #'(lambda () (td-packages-cleanup td-package-manifest)))

(provide 'td-packages)
;;; td-packages.el ends here
