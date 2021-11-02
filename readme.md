- [Emacs Configuration](#org8bf915b)
  - [Performance Tweaks](#orgb015b48)
  - [Autosaves & Backups](#orgbac89f7)
  - [Package Management](#org003231c)
  - [Look & Feel](#org31f8a4e)
    - [Dired](#orgfb6d043)
    - [DOOM](#org7baec53)
    - [Emacs](#orgd6ee9cf)
    - [Font Setup](#org6dbac52)
  - [Utility Packages](#orgd6e8e85)
    - [AG](#org46e7ef0)
    - [Diff-hl](#org317f31b)
    - [Evil!](#orgcb29de1)
    - [Flycheck Aspell](#org25d6367)
    - [Magit](#org7a7b105)
    - [Mu4e](#orgb870714)
    - [Ivy](#orgbc16ba4)
    - [Projectile](#org3983fba)
    - [Undo-Tree](#org275af0e)
    - [VTerm](#org7659dc6)
    - [Which-key](#org8ee8795)
  - [Syntax Support](#orgfea6d2b)
    - [Company Completions](#org0d4b55b)
    - [CSS/SCSS](#orgf9a2013)
    - [Emmet](#orgf2e88a8)
    - [Flycheck](#orgc232374)
    - [LSP](#orgb2a25cc)
    - [Markdown](#orgd2369c5)
    - [Org](#org7e6bde1)
    - [Prettier](#org9e2a4aa)
    - [Prog Mode](#org5427571)
    - [Python](#org532dd41)
    - [Rainbow Delimiters](#org75c0f5f)
    - [Rainbow Mode](#org9598022)
    - [Shopify Mode](#orgc787a6a)
    - [Svelte](#org62d0af4)
    - [Treesitter](#orgaa40249)
    - [TypeScript & JavaScript](#org6eb2008)
    - [VueJS](#orgd18cf91)
    - [Web Mode](#org20a8f11)
    - [YAML](#org10954f1)
    - [Yasnippet](#orgc234446)


<a id="org8bf915b"></a>

# Emacs Configuration

Herein lies my Emacs configuration. Use it well


<a id="orgb015b48"></a>

## Performance Tweaks

Give the garbage collector free-reign at start-up, then take it back to 8M. Use so-long-mode for editor-breaking minified css/js.

```emacs-lisp
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(setq read-process-output-max (* 1024 1024))

;; Don't die when handling large, minified files
(global-so-long-mode 1)
```


<a id="orgbac89f7"></a>

## Autosaves & Backups

I hate seeing project folders get all cluttered up

```emacs-lisp
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
```


<a id="org003231c"></a>

## Package Management

We need packages, so we're going to setup some repos and pre-install use-package for packages later on in this config

```emacs-lisp
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
```


<a id="org31f8a4e"></a>

## Look & Feel


<a id="orgfb6d043"></a>

### Dired

```emacs-lisp
(add-hook 'dired-mode-hook (lambda()
                             (dired-hide-details-mode)))

(use-package all-the-icons-dired
  :requires all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))
```


<a id="org7baec53"></a>

### DOOM

/"Against all the evil that Hell can conjure, all the wickedness that mankind can produce, we will send unto them&#x2026; only you. Rip and tear, until it is done."/

```
                   ..
         ..        ::            ..
         ..        :-:
                   .::
                     ..    ......
                 ..::::--:---:----:
             ..::::::-:::.     ..::
                     :-:-.
                      -::    .
                      :::    :.
                :     .-:    ::
                :.    :-:   :-:
                .:    .-:..-:
                  :   :-:-:.
                      :-::
         ...    ...:--::-         .
          ..     :-:. :-:       ...
                      .-:
                       :.
                       :.
                       .
```

Doom's themes and modeline are hard to beat. They're easy to install, highly customizable and hackable. Writing my own theme is easy.

```emacs-lisp
(use-package all-the-icons :ensure t)

;; I will write/store my custom doom (and non-doom?) themes here
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(use-package doom-themes
  :ensure t
  :requires all-the-icons
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

```


<a id="orgd6ee9cf"></a>

### Emacs

Clear out most of the GUI clutter, display relative line numbers, highlight the line I'm on, smaller left-only fringe, quick yes/no answers, some prog-mode QOL settings as well

```emacs-lisp
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)
(fringe-mode '(4 . 0))
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(defalias 'yes-or-no-p 'y-or-n-p)
(setq browse-url-generic-program (executable-find "firefox"))
```


<a id="org6dbac52"></a>

### Font Setup

Need UTF-8 for programming, would also like to enjoy Fira Code with ligatures.

```emacs-lisp
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(add-to-list 'default-frame-alist '(font . "Source Code Pro Semibold 10" ))
```

Ligature support currently disabled due to unexpected side effects with [doom-modeline](https://github.com/seagle0128/doom-modeline/issues/465). If I feel like this is important enough to fix I'll do it.

```
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font 10" ))
(require 'ligature)
;; Enable ligatures in programming modes
(ligature-set-ligatures 'prog-mode '(
  "www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
  ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
  "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
  "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
  "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
  "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
  "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
  "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
  "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
  "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")
)

(add-hook 'prog-mode-hook #'ligature-mode)
```


<a id="orgd6e8e85"></a>

## Utility Packages

Packages that extend and augment emacs in a general way


<a id="org46e7ef0"></a>

### AG

C based file content grepping <3

```emacs-lisp
(use-package ag :ensure t)
```


<a id="org317f31b"></a>

### Diff-hl

Show me the diffs in the fringe!

```emacs-lisp
(use-package diff-hl
  :ensure t
  :after magit
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))
```


<a id="orgcb29de1"></a>

### Evil!

I like Vim bindings. Many of the plugins included here are commonly enjoyed in Vim. Most of them can be looked up [in the wiki](https://www.emacswiki.org/emacs/Evil#h5o-6).

```emacs-lisp
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-shift-width tab-width)
  (evil-mode 1))

;; Easily wrap selections/motions in pairs
(use-package evil-surround
  :requires evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; A collection of vim-rebinds that evil doesn't cover
(use-package evil-collection
  :requires evil
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Org mode keybind improvements
(use-package evil-org
  :requires evil
  :ensure t
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Highlight text when yanked, popped, deleted or changed with motions. Helps me visually grok what a motion has done.
(use-package evil-goggles
  :requires evil
  :ensure t
  :custom-face
  (evil-goggles-delete-face ((t (:foreground "#620707" :background "#F57373"))))
  (evil-goggles-paste-face ((t (:foreground "#426214" :background "#C3E88D"))))
  (evil-goggles-default-face ((t (:foreground "#002A82" :background "#82AAFF"))))
  :config
  (evil-goggles-mode))

;; Easy commenting
(use-package evil-commentary
  :requires evil
  :ensure t
  :config (evil-commentary-mode))

;; Think Ace Jump, but it's Vim. I've intentionally set the scope to much larger than the default of "after where you are on this line".
(use-package evil-snipe
  :requires evil
  :ensure t
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (setq evil-snipe-scope 'whole-visible)
  (setq evil-snipe-spillover-scope 'whole-buffer))
```


<a id="org25d6367"></a>

### Flycheck Aspell

This requires [flycheck](#orgc232374) to be loaded and ready. This permits spell checking while writing documentation on the fly.

```emacs-lisp
(defun flycheck-maybe-recheck (_)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

(use-package flycheck-aspell
  :after flycheck
  :ensure t
  :custom
  (ispell-personal-dictionary "~/.config/emacs/aspell.en.pwd")
  :config
  (flycheck-aspell-define-checker "org"
                                  "Org" ("--add-filter" "url")
                                  (org-mode))
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'org-aspell-dynamic)
  (advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck))
```


<a id="org7a7b105"></a>

### Magit

Magit is one of the biggest reasons why I fell in love with emacs. It's the best keyboard driven "TUI" abstraction of the git command line anywere, period. Better than Fugitive by far. Sorry, Tim Pope.

```emacs-lisp
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))
```


<a id="orgb870714"></a>

### Mu4e

Setting up mu4e with contexts feels like a pretty massive process. I've decided to load that config from an external file to keep this file sane. Also, I've decided not to expose my email addresses/configs here. Apologies for those who may be looking for an example!

If you're looking for help with mu/4e I strongly suggest checking out [System Crafters](https://www.youtube.com/watch?v=yZRyEhi4y44&list=PLEoMzSkcN8oM-kA19xOQc8s0gr0PpFGJQ)

```emacs-lisp
(load-file "~/.config/emacs/mu4e-config.el")
```


<a id="orgbc16ba4"></a>

### Ivy

Nicer command completions for emacs. Not as bloated as Helm. Ivy requires 3 packages

```emacs-lisp
;; Completions with counsel
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

;; Search better with swiper
(use-package swiper
  :ensure t
  :config
  (counsel-mode 1))

;; The interface for swiper/counsel
(use-package ivy
  :ensure t
  :requires (counsel swiper)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x))
```


<a id="org3983fba"></a>

### Projectile

Project management. Makes it very easy to quickly switch to a project root (folder with git initialized) and then quickly pull up files or search file contents.

```emacs-lisp
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-systen 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p s s") 'counsel-ag)
  (define-key projectile-mode-map (kbd "C-c p s r") 'counsel-rg)
  (define-key projectile-mode-map (kbd "C-c p s g") 'counsel-grep-or-swiper)
  (setq projectile-project-search-path '(("~/Projects/" . 2) "/srv/www")))
```


<a id="org275af0e"></a>

### Undo-Tree

Mostly want undo tree for better redo support for Evil

```emacs-lisp
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))
```


<a id="org7659dc6"></a>

### VTerm

A real-ish terminal for emacs that doesn't make me hurt inside. Requires `libvterm`. [Documentation is here](https://github.com/jixiuf/emacs-libvterm). This needs to be installed & compiled *after* magit, because for [whatever reason](https://github.com/magit/with-editor/issues/86), attempting to install vterm first breaks magit.

```emacs-lisp
(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm))
```


<a id="org8ee8795"></a>

### Which-key

What the heck was that keybind again? If you can remember how it starts, which-key can help you find the rest.

```emacs-lisp
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
```


<a id="orgfea6d2b"></a>

## Syntax Support

We're getting into to the language specific stuff now. Much of this is specifically tailored for Shopify, TypeScript and JavaScript development. Many if not all of these features stay out of the way when you're not in the language mode. There's also a very tedious attempt to make all of these disjointed program modes listen to my gosh dang tab-width setting instead of doing their own thing as an insane default.


<a id="org0d4b55b"></a>

### Company Completions

Completions at point!

```emacs-lisp
(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-backends '((company-files company-keywords company-capf company-yasnippet)
                           (company-abbrev company-dabbrev)))
  :custom
  (company-idle-delay 0.0))
```


<a id="orgf9a2013"></a>

### CSS/SCSS

```emacs-lisp
(setq-default css-indent-level tab-width)
(setq-default css-indent-offset tab-width)
```


<a id="orgf2e88a8"></a>

### Emmet

`.Emmet[data-love`"true"]=

```emacs-lisp
(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-expand-jsx-className t)
  :hook
  (sgml-mode-hook . emmet-mode)
  (css-mode-hook . emmet-mode)
  (web-mode . emmet-mode)
  (svelte-mode . emmet-mode))
```


<a id="orgc232374"></a>

### Flycheck

Linting, mostly for lsp-mode :D. I did not like the default fringe markers as the expect a minimum fringe of 8-16px, so I followed a [tip on how to change up the marker](https://emacs.stackexchange.com/questions/36363/how-to-change-flycheck-symbol-like-spacemacs#answer-36373) for a 4px fringe.

```emacs-lisp
(define-fringe-bitmap 'flycheck-fringe-bitmap-caret
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b10000000
          #b11000000
          #b11100000
          #b11110000
          #b11100000
          #b11000000
          #b10000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000))

(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode)
  :bind ("C-c f" . flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold 1000)
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-caret
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)
  (flycheck-define-error-level 'warning
    :severity 100
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-caret
    :fringe-face 'flycheck-fringe-warning
    :warning-list-face 'flycheck-warning-list-warning)
  (flycheck-define-error-level 'info
    :severity 100
    :compilation-level 1
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-caret
    :fringe-face 'flycheck-fringe-info
    :info-list-face 'flycheck-info-list-info))
```


<a id="orgb2a25cc"></a>

### LSP

The meat and potatoes of the modern text editor turned IDE, all thanks to Microsoft doing an open source thing with VSCode.

```emacs-lisp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (css-mode . lsp)
         (go-mode . lsp)
         (html-mode . lsp)
         (js-mode . lsp)
         (json-mode . lsp)
         (python-mode . lsp-deferred)
         (rjsx-mode . lsp)
         (rust-mode . lsp)
         (scss-mode . lsp)
         (shopify-mode . lsp)
         (svelte-mode . lsp)
         (typescript-mode . lsp)
         (vue-mode . lsp)
         (yaml-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . (lambda()
                       (display-line-numbers-mode)
                       (hl-line-mode))))
  :custom
  ;; Please don't log garbage in my project folders
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :commands lsp)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\]env' [/\\]__pycache__'"))

(use-package lsp-ui
  :requires lsp-mode
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :requires ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
```


<a id="orgd2369c5"></a>

### Markdown

The free software documentation language of the Internet.

```emacs-lisp
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
```


<a id="org7e6bde1"></a>

### Org

Customizations for what is one of the best features that emacs comes with. If we weren't so hung up on Markdown for developer docs, I'd use this mode for everything doc related. Yes, I can export an org file. If I want to track two files, that is. The fancy font-size setup is stolen from [Emacs from Scratch](https://github.com/daviwil/emacs-from-scratch) by [System Crafters](https://www.youtube.com/c/SystemCrafters). Check them out :)

```emacs-lisp
(use-package org
  :ensure t
  :hook (org-mode . (lambda()
                      (org-indent-mode)
                      (yas-minor-mode)
                      (org-clock-persistence-insinuate)))
  :config
  (require 'ox-md nil t)
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-directory "~/Org")
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-src-preserve-indentation t)
  (org-clock-persist 'history)
  (org-agenda-block-separator "──────────")
  (org-agenda-files '(
                      "~/Org"
                      "~/Org/Voltage"))
  (org-tag-alist '(
                   (:startgroup)
                   ; Put mutually exclusive tags here
                   (:endgroup)
                   ("@home" . ?H)
                   ("@work" . ?W)
                   ("urgent" . ?U)
                   ("learning" . ?l)
                   ("foss" . ?f)
                   ("blog" . ?b)
                   ("idea" . ?i)))
  (org-todo-keywords `((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                       (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (org-agenda-custom-commands '(("d" "Dashboard"
                                 ((agenda "" ((org-deadline-warning-days 7)))
                                  (todo "NEXT"
                                        ((org-agenda-overriding-header "Next Tasks")))
                                  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

                                ("n" "Next Tasks"
                                 ((todo "NEXT"
                                        ((org-agenda-overriding-header "Next Tasks")))))

                                ("U" "Urgent Tasks" tags-todo "+urgent")

                                ;; Low-effort next actions
                                ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                                 ((org-agenda-overriding-header "Low Effort Tasks")
                                  (org-agenda-max-todos 20)
                                  (org-agenda-files org-agenda-files)))

                                ("w" "Workflow Status"
                                 ((todo "WAIT"
                                        ((org-agenda-overriding-header "Waiting on External")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "REVIEW"
                                        ((org-agenda-overriding-header "In Review")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "PLAN"
                                        ((org-agenda-overriding-header "In Planning")
                                         (org-agenda-todo-list-sublevels nil)
                                         (org-agenda-files org-agenda-files)))
                                  (todo "BACKLOG"
                                        ((org-agenda-overriding-header "Project Backlog")
                                         (org-agenda-todo-list-sublevels nil)
                                         (org-agenda-files org-agenda-files)))
                                  (todo "READY"
                                        ((org-agenda-overriding-header "Ready for Work")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "ACTIVE"
                                        ((org-agenda-overriding-header "Active Projects")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "COMPLETED"
                                        ((org-agenda-overriding-header "Completed Projects")
                                         (org-agenda-files org-agenda-files)))
                                  (todo "CANC"
                                        ((org-agenda-overriding-header "Cancelled Projects")
                                         (org-agenda-files org-agenda-files))))))))

(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '(
                             ;;"◉" "○" "•" "·"
                             "" "❯" "»" "›"
                             ))
  (org-ellipsis " ")
  :config
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'semi-bold :height (cdr face)))
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package ox-gfm
  :requires org
  :ensure t)

(defun org-visual-fill-setup()
  "Center the column 100 characters wide"
  (setq-local visual-fill-column-width 100
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . org-visual-fill-setup))

(use-package org-alert
  :ensure t
  :custom
  (alert-default-style 'libnotify)
  :config
  (org-alert-enable))

(use-package org-trello
  :requires org
  :ensure t
  :custom
  (org-trello-files '(
                      "~/Org/Voltage/Trello/kytebaby.trello.org"
                      "~/Org/Voltage/Trello/resthouse.trello.org")))
```


<a id="org9e2a4aa"></a>

### Prettier

An opinionated way to clean up my web-dev code quickly.

```emacs-lisp
(use-package prettier-js
  :ensure t)
```


<a id="org5427571"></a>

### Prog Mode

A few settings that are useful in programming buffers

```emacs-lisp
(defun toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (setq-local indent-tabs-mode (not indent-tabs-mode)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  "))
        (tab-count (how-many "^\t")))
    (if (> space-count tab-count)
        (setq indent-tabs-mode nil))
    (if (> tab-count space-count)
        (setq indent-tabs-mode t))))

;; I want a way to tab over relative similar tab-to-tab-stop
(global-set-key (kbd "C-i") 'indent-relative)

(setq standard-indent 2)
(setq backward-delete-char-untabify-method 'hungry)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default electric-indent-inhibit t)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq whitespace-style '(face tabs tab-mark trailing))
            (setq-local fill-column 80)
            (setq-local show-trailing-whitespace t)
            (display-line-numbers-mode)
            (hl-line-mode)
            (display-fill-column-indicator-mode)
            (electric-pair-local-mode)
            (local-set-key (kbd "<RET>") 'newline-and-indent)
            (yas-minor-mode)
            (infer-indentation-style)
            (setq-local whitespace-display-mappings
                        '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
            (whitespace-mode)))
```


<a id="org532dd41"></a>

### Python

<3 Python

```emacs-lisp
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))

(use-package pyvenv :ensure t)
(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode))
```


<a id="org75c0f5f"></a>

### Rainbow Delimiters

This comes in handier than you think it would. Especially with these (lisp '((config . files)))

```emacs-lisp
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
```


<a id="org9598022"></a>

### Rainbow Mode

LSP-Mode covers making visual representations of hex color codes almost everywhere I need it. For everywhere else there's rainbow-mode

```emacs-lisp
(use-package rainbow-mode :ensure t)
```


<a id="orgc787a6a"></a>

### Shopify Mode

This is where I turn emacs into a usuable IDE for Shopify themes. I use regexp to tell emacs to use s/css-mode for css liquid, then register an LSP client for the [theme-check-language-server](https://shopify.dev/themes/tools/theme-check#using-theme-check-in-other-editors).

```emacs-lisp
                                        ; Derive liquid-mode from web-mode
(define-derived-mode shopify-mode web-mode "Shopify"
  "Use web mode to highlight shopify liquid files")
(provide 'shopify-mode)

(add-to-list 'auto-mode-alist '("\\.liquid\\'" . shopify-mode))

(defvar liquid-electric-pairs '((?% . ?%)) "Electric pairs for liquid syntax.")

(defun liquid-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs liquid-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'shopify-mode-hook #'liquid-add-electric-pairs)

;; Shopify template lsp with theme-check
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(shopify-mode . "shopify"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "theme-check-language-server")
                    :activation-fn (lsp-activate-on "shopify")
                    :server-id 'theme-check)))
```


<a id="org62d0af4"></a>

### Svelte

Fake-out a "svelte-mode" for the purposes of activating lsp-mode with the svelte-language-server. I'm extending web-mode because it highlights `.svelte` files well.

```emacs-lisp
(define-derived-mode svelte-mode web-mode "Svelte"
  "I just want web-mode highlighting with .svelte files")
(provide 'svelte-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
```


<a id="orgaa40249"></a>

### Treesitter

Tree-sitter is an impressive project. It delivers exceptionally rich syntax highlighting for things like emacs/vim. A little tricky to theme, though, as it has a billion font lock faces and every tree-sitter syntax config may or may not use them the same way. I try to avoid looking a gift horse in the mouth.

```emacs-lisp
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs :ensure t)
```


<a id="org6eb2008"></a>

### TypeScript & JavaScript

Use tide-mode for all things TS/JS.

```emacs-lisp
(defun setup-tide-mode()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq tide-format-options '(:indentSize 2 :tabSize: 2))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Syntax highlighting starts with rjsx-mode before it's made better by tree sitter
(use-package rjsx-mode :ensure t)

(use-package tide
  :ensure t
  :after
  (rjsx-mode company flycheck)
  (typescript-mode company flycheck)
  :hook (typescript-mode . setup-tide-mode)
  (js-mode . setup-tide-mode)
  (rjsx-mode . setup-tide-mode))

(setq js-indent-level tab-width)
(setq typescript-indent-level tab-width)
```


<a id="orgd18cf91"></a>

### VueJS

```emacs-lisp
(define-derived-mode vue-mode web-mode "VueJS"
  "I just want web-mode highlighting with .svelte files")
(provide 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
```


<a id="org20a8f11"></a>

### Web Mode

There isn't a much better catch-all for web template syntax support than web-mode. It works well with Liquid syntax files. It also comes with it's own divergent, insane defaults that I have to choke out.

```emacs-lisp
(defun customize-web-mode ()
  "Customizations for web mode"
	(setq web-mode-enable-auto-pairing nil
				web-mode-enable-auto-quoting nil
				web-mode-enable-auto-indentation nil
				web-mode-style-padding tab-width
				web-mode-script-padding tab-width)
	(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
	(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
	(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
	(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

(use-package web-mode
  :ensure t
  :hook
	(web-mode . customize-web-mode)
  :mode
  ("\\.html\\'" . web-mode))
```


<a id="org10954f1"></a>

### YAML

YAML's a really nice way to configure software, containers and projects. I use it when I can.

```emacs-lisp
(use-package yaml-mode
  :ensure t)
```


<a id="orgc234446"></a>

### Yasnippet

Snippets! They're helpful.

```emacs-lisp
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs `(
                           "~/.config/emacs/yasnippets"))
  (yas-reload-all))

(use-package yasnippet-snippets :ensure t)
```
