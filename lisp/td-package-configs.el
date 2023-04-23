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

(require 'td-packages)
(require 'td-helpers)

;;; Priority Mode
(require 'priority-mode)
(priority-mode)

;;; mood-line
(mood-line-mode)

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
(define-key priority-mode-map (kbd "C-:") #'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "C-:") #'avy-isearch)
(avy-setup-default)

;;; Minadverse
;; Completions provided by one pretty brilliant dev. Thanks Minad!
;;
;; Snippets
(defun tempel-setup-capf ()
  "Add the Tempel Capf to `completion-at-point-functions'."
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

;; Completions
(require 'corfu)
(setq corfu-cycle t
      corfu-auto t
      corfu-auto-prefix 3
      corfu-auto-delay 0.3)

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(global-corfu-mode 1)

(unless (or (daemonp) (display-graphic-p))
  (corfu-terminal-mode 1))

(with-eval-after-load 'god-mode
  (add-hook 'god-local-mode-hook #'corfu-quit))

(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'text-mode-hook
          #'(lambda ()
              (add-to-list 'completion-at-point-functions #'cape-ispell)))

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t
                                 corfu-auto nil)
            (corfu-mode)))

;;; dap-mode
(with-eval-after-load 'lsp-mode
  (dap-auto-configure-mode))

;;; Direnv
(direnv-mode)

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

;;; diff-hl
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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

;;; eshell:
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)

;;; Expand Region:
(defun td-setup-expand-region ()
  "Set up expand region."
  (require 'expand-region)
  (td-bind-keys '(("C-=" . er/expand-region)))
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
  (define-key priority-mode-map (kbd "C-,") er/keymap))

(add-hook 'after-init-hook #'td-setup-expand-region)

;;; Goggles
(td-add-hooks '(text-mode prog-mode) #'goggles-mode)
(setq-default goggles-pulse t)

;;; Highlight Indent Guides
(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-auto-character-face-perc 25
      highlight-indent-guides-responsive 'top)

;;; Ledger
(setq ledger-use-native-highlighting t)

;;; Magit
(global-set-key (kbd "C-c m") #'magit-status)

;;; Multiple Cursors
(td-bind-keys '(("C-S-l"   . mc/edit-lines)
                ("C->"     . mc/mark-next-like-this)
                ("C-<"     . mc/mark-previous-like-this)
                ("C-M->"   . mc/skip-to-next-like-this)
                ("C-M-<"   . mc/skip-to-previous-like-this)
                ("C-c C-?" . mc/mark-all-like-this-dwim)
                ("C-c C-/" . mc/mark-all-in-region)
                ("C-M-n"   . mc/insert-numbers)
                ("C-M-a"   . mc/insert-letters))
              priority-mode-map)

;;; Pinentry
(pinentry-start)

;;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;; Sensitive Mode
(require 'sensitive-mode)

;;; RG
(rg-enable-default-bindings)

;;; Transpose Mark
(global-set-key (kbd "C-c t") #'transpose-mark)

;;; Which Key
(which-key-mode)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

(provide 'td-package-configs)
;;; td-package-configs.el ends here