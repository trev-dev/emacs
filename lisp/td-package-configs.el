;;; td-package-configs.el --- Package management -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file contains multiple package configurations that are best categorized
;; as "miscellaneous."  There are a lot of packages that only need a small
;; handful of lines to set up.  Using `require' is expensive.  All of the
;; small and trivial configurations are here.
;;
;; Larger configurations have their own file.
;;
;;; Code:

(require 'td-package-manifest)
(require 'td-helpers)

;;; Priority Mode
(require 'priority-mode)
(priority-mode)

;;; Anzu
(with-eval-after-load 'isearch
  (require 'anzu)
  (global-anzu-mode +1)

  (setq anzu-mode-lighter ""
        anzu-deactivate-region t
        anzu-search-threshold 1000
        anzu-replace-threshold 50
        anzu-cons-mode-line-p nil)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))

;;; Avy
(define-key priority-mode-map (kbd "C-c ;") #'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "C-c ;") #'avy-isearch)
(avy-setup-default)

;;; Company
(add-hook 'after-init-hook #'global-company-mode)

;;; dap-mode
(with-eval-after-load 'lsp-mode
  (dap-auto-configure-mode)
  (require 'dap-java))

;;; Diminish
(defun td-diminish-lsp-lighter ()
  "Display the LSP status in the `mode-line-modes'."
  (let* ((lsp-up lsp--buffer-workspaces)
         (color (if lsp-up '(:inherit success :weight bold)
                  '(:inherit warning :weight bold))))
    `(:propertize " LSP" face ,color)))

(dolist (mode '(("company" 'company-mode)
                ("hideshow" 'hs-minor-mode)
                ("undo-tree" 'undo-tree-mode)
                ("whitespace" 'whitespace-mode)
                ("yasnippet" 'yas-minor-mode)
                ("which-key" 'which-key-mode)
                ("org-indent" 'org-indent-mode)
                ("simple" 'visual-line-mode)
                ("eldoc" 'eldoc-mode)
                ("lsp-mode" 'lsp-mode '(:eval (td-diminish-lsp-lighter)))
                ("beacon" 'beacon-mode)
                ("goggles" 'goggles-mode)))
  (eval-after-load (car mode)
    `(diminish ,(cadr mode) ,(caddr mode))))

(diminish 'defining-kbd-macro)

(with-eval-after-load 'meow
  (dolist (mode (list 'meow-normal-mode
                      'meow-insert-mode
                      'meow-motion-mode
                      'meow-keypad-mode
                      'meow-beacon-mode))
    (diminish mode)))

;;; Goggles
(add-hook 'text-mode-hook #'goggles-mode)
(add-hook 'prog-mode-hook #'goggles-mode)

;;; Exec Path From Shell
(require 'exec-path-from-shell)
(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG"))
  (add-to-list 'exec-path-from-shell-variables var))
(exec-path-from-shell-initialize)

;;; Ligature
(with-eval-after-load 'ligature
  (ligature-set-ligatures
   'prog-mode
   '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
     (";" (rx (+ ";")))
     ("&" (rx (+ "&")))
     ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
     ("?" (rx (or ":" "=" "\." (+ "?"))))
     ("%" (rx (+ "%")))
     ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]" "-" "=" ))))
     ("\\" (rx (or "/" (+ "\\"))))
     ("+" (rx (or ">" (+ "+"))))
     (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
     ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
     ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
     ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
     ("*" (rx (or ">" "/" ")" (+ "*"))))
     ("w" (rx (+ "w")))
     ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
     (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
     ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_" (+ "#"))))
     ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
     ("_" (rx (+ (or "_" "|"))))
     ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
     "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
     "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))
(global-ligature-mode t)

;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless basic)
        completion-category-overrides
        '((file (styles basic partial-completion))))

;;; Vertico with Marginalia
(savehist-mode)
(vertico-mode)
(marginalia-mode)

;;; Visual Fill
(add-hook 'org-mode-hook #'(lambda ()
                             (setq-local fill-column 100)
                             (visual-fill-column-mode)))

;;; Dired
(setq dired-dwim-target t)
(with-eval-after-load 'dired
  (require 'all-the-icons)
  (setq all-the-icons-dired-monochrome nil)
  (add-hook
   'dired-mode-hook #'(lambda ()
                        (when (display-graphic-p)
                          (all-the-icons-dired-mode))
                        (dired-hide-details-mode)))

  (autoload 'gnus-dired-attach "gnus-dired.el"
    "Attach files from Dired to the current Message buffer." t)
  (define-key dired-mode-map (kbd "C-c C-m C-a") #'gnus-dired-attach))

;;; Direnv
(direnv-mode t)

;;; diff-hl
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; Eldoc
(setq eldoc-echo-area-use-multiline-p nil)

;;; ERC
(setq erc-autojoin-channels-alist
      '(("Libera.Chat" "#emacs" "#systemcrafters" "#stumpwm")))

(defun td-launch-erc ()
  "Launch the ERC IRC client andlg in."
  (interactive)
  (erc-tls :server "irc.libera.chat"
                             :port 7000
                             :nick "trevdev"
                             :password (password-store-get
                                        "Biz/libera.chat")))

;;; eshell
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)

;;; Expand Region:
(defun td-setup-expand-region ()
  "Set up expand region."
  (require 'expand-region)
  n(td-bind-keys '(("C-c =" . er/expand-region)))
  (defvar er/keymap
    (let ((map (make-sparse-keymap "er/objects")))
      (td-bind-keys '(("w"   . er/mark-word)
                      ("W"   . er/mark-symbol)
                      ("s"   . er/mark-sentence)
                      ("p"   . er/mark-paragraph)
                      ("e"   . er/mark-email)
                      ("d"   . er/mark-defun)
                      ("u"   . er/mark-url)
                      ("o p" . er/mark-outside-pairs)
                      ("i p" . er/mark-inside-pairs)
                      ("o s" . er/mark-outside-quotes)
                      ("i s" . er/mark-inside-quotes)
                      ("o e" . er/mark-org-element)
                      ("o E" . er/mark-org-element-parent))
                    map)
      map)
    "A keymap for quickly calling expand region functions.
\\{er/keymap}")
  (fset 'er/keymap er/keymap)
  (define-key priority-mode-map (kbd "C-c ,") er/keymap))

(add-hook 'after-init-hook #'td-setup-expand-region)

;;; Highlight Indent Guides
(setq highlight-indent-guides-method 'character
      highlight-indent-guides-auto-character-face-perc 25
      highlight-indent-guides-responsive 'top)

;;; Ledger
(setq ledger-use-native-highlighting t)

;;; Magit
(global-set-key (kbd "C-c m") #'magit-status)

;;; Pinentry
(pinentry-start)

;;; Project
(setq project-vc-extra-root-markers '("pom.xml" "package.json"))

;;; Sensitive Mode
(require 'sensitive-mode)

;;; RG
(rg-enable-default-bindings)

;;; Which Key
(which-key-mode)

;;; Yasnippet
(with-eval-after-load 'yasnippet
  (global-set-key (kbd "C-c ,") #'yas-expand)
  (setq yas-snippet-dirs '("~/.emacs.d/yasnippets"))
  (yas-reload-all))

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

(provide 'td-package-configs)
;;; td-package-configs.el ends here
