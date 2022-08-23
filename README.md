# Emacs Configuration


The following document is a literate program that will generate my `~/.emacs.d`. It is intended to be used in conjunction with a dotfile manager such as GNU Stow for symlinking back to the home directory. You could, however, tangle this file from the home directory and skip using stow.

To read more about how this configuration is supposed to work, see my [config strategy](#org6295672).

**Table of Contents**

1.  [Configuration Strategy](#org6295672)
    1.  [Tangling Files](#orge605170)
    2.  [Configuring Packages](#org7dbd41b)
2.  [Early Init](#org4888d61)
3.  [Init File Headers](#orgdf1f1b2)
4.  [General Settings](#org8ec129b)
5.  [Helper Functions](#org29eebb2)
    1.  [Priority Mode](#orgd5229f7)
6.  [Keybinds](#org85e1607)
7.  [Prog Mode](#org743aaa9)
8.  [Package Configuration](#org6cb540c)
    1.  [Bootstrapping](#org9c6ad8c)
        1.  [Repositories](#org073f7e8)
        2.  [Maintaining selected packages](#orge50e6f7)
        3.  [Quelpa](#org6ab3085)
    2.  [Look & Feel](#orga25570a)
        1.  [All The Icons](#org65b1e51)
        2.  [Dired](#org58af203)
        3.  [Diminish](#org4e23164)
        4.  [Custom Theme](#orgce1c43c)
        5.  [Font Setup](#org258b65a)
        6.  [Modeline](#org98dc871)
    3.  [Utility Packages](#org9741e49)
        1.  [Avy](#org6fb1fd4)
        2.  [CTRLF](#org36c8af6)
        3.  [Completions](#orgf8f22c5)
            1.  [Cape](#org1c4f19e)
            2.  [Consult](#org41636f3)
            3.  [Corfu](#org561f111)
            4.  [Fussy](#orge94f252)
            5.  [Kind-Icon](#org6026003)
            6.  [Marginalia](#org0b4a8bb)
            7.  [Savehist](#org72754b3)
            8.  [TempEl](#org87e60db)
            9.  [Vertico](#org612190b)
        4.  [Dashboard](#org7f82336)
        5.  [Diff-hl](#orgf779f48)
        6.  [Elfeed](#org06d85c9)
        7.  [Surround](#orgda59bcf)
        8.  [ERC](#org0fdb0ed)
        9.  [Eshell](#org3384f1b)
        10. [Expand Region](#orgdd33a0a)
        11. [God Mode](#orge743755)
            1.  [Functions](#org1857d14)
            2.  [Insert Ahead](#orgd084905)
            3.  [Org Mode Newline Advice](#org2462356)
            4.  [Seeking Characters](#org4d72ca6)
            5.  [Cursor Indicator](#orgac39169)
            6.  [Keybindings](#org73dc584)
            7.  [Apply & Finish Setup](#orgba3bf68)
        12. [Goggles](#orgd3abddd)
        13. [Magit](#org4c11dfa)
        14. [Multiple Cursors](#orgddb3094)
        15. [Org](#orgde09f37)
            1.  [Key Variables](#org75f75ed)
            2.  [Functions](#org5b841eb)
            3.  [Apply Configuration](#org7cb8bb5)
            4.  [Extending Org Mode](#orgd8318e6)
            5.  [Custom Clock Table](#org429f8cb)
        16. [Ledger](#orgc4f2f2e)
        17. [Vterm](#org60a40c3):guix:
        18. [Notmuch](#org406badd)
            1.  [Built In Mail Settings](#org8d77534)
            2.  [Notmuch](#org87e7497)
            3.  [org-mime](#org2ff58b1)
            4.  [org-contacts](#orgc17e144)
        19. [Password Store](#org1219773)
        20. [Sensitive Mode](#org70cffe1)
        21. [RG](#org3d118a8)
        22. [Visual Fill Column](#org330c0c4)
        23. [Which-key](#org7287c57)
        24. [Windmove](#org4d282eb)
    4.  [Syntax Support](#org113a830)
        1.  [Clojure](#org86100c5)
        2.  [Common Lisp](#orge51cb28)
        3.  [CSS/SCSS](#org22a9399)
        4.  [Eglot](#org3e77fed)
        5.  [Eldoc](#orgf1e2c3b)
        6.  [Eldoc Box](#org566ffff)
        7.  [Emmet](#org2f6640b)
        8.  [GoLang](#orge3f707c)
        9.  [Lua Mode](#orgb3f7081)
        10. [Markdown](#orgf76b6ce)
        11. [Nim](#orged0968e)
        12. [Paredit](#org631651a)
        13. [PHP](#orgf23f861)
        14. [Prettier](#org3587ae1)
        15. [Python](#orgf871173)
        16. [Rainbow Delimiters](#org3026418)
        17. [Rainbow Mode](#orgd5d861d)
        18. [Ruby](#orgb00386c)
        19. [Rust](#org6f89698)
        20. [Scheme](#org53f5973)
        21. [Shopify Mode](#org43eb35e)
        22. [Svelte](#orgcb67f27)
        23. [Treesitter](#org848b8bc)
        24. [TypeScript & JavaScript](#org4c1c99f)
        25. [VueJS](#org7004dc7)
        26. [Web Mode](#org13aa50a)
        27. [YAML](#org9fc16ea)
    5.  [Load Customizer Settings](#org991b107)
9.  [About This Config](#org6508ba8)
    1.  [Installation](#orgd63c4c1)
    2.  [Licenses](#orgdf2956e)


<a id="org6295672"></a>

## Configuration Strategy

The goal with this configuration is to generate simple, elisp output that will configure Emacs.

In the past I have used plain elisp files. As they grow they become increasingly harder to follow. I switched org-babel and added that directly into my `init.el`. This works OK, but I never leveraged the full power of babel to create a tight configuration.

At the time of my writing this, my configuration is somewhat convoluted. I aim to simplify the elisp output and make it easier to debug.


<a id="orge605170"></a>

### Tangling Files

I will be using `org-babel-tangle` to generate numerous files and to help separate concerns. Some files that this configuration should generate are the `early-init.el`, `init.el` and my `package-list.el`. Other files may be generated if it makes sense to document them here.


<a id="org7dbd41b"></a>

### Configuring Packages

I will be using `noweb-ref` header arguments and the `noweb` feature to keep packages organized and separate from one-another. I want it to be a simple matter for me to exclude a package from both my `package-selected-packages` and my `init.el` by commenting out it's headline.

Each package will have 2 source blocks.

The first block will add the package to the `td/selected-packages` variable by tangling the package name to the `emacs.d/package-list.el` file. This file is loaded early in `init.el`.

The second block will contain the actual configuration for the package or built-in feature. This block will be evaluated later on in the `init.el` file.


<a id="org4888d61"></a>

## Early Init

This file is loaded before the initialization of emacs begins. It is sometimes helpful to pre-configure stuff in here.

**Note:** The `:PROPERTIES:` drawer in this section flags org-babel to make sure that the `.emacs.d/` directory exists using the `:mkdirp yes` argument.

```elisp
;;; early-init.el --- Emacs early-init setup.
;;
;;; Commentary:
;; This file was generated using literate programming. Please see the config.org
;; file.
;;
;;; Code:
```

We want the garbage collector to have no limit during the init sequence.

```elisp
(setq gc-cons-threshold most-positive-fixnum)
```

```elisp
;;; early-init.el ends here
```


<a id="orgdf1f1b2"></a>

## Init File Headers

This generates the top of the init file, which will set up the lexical scope and describe to Emacs what the file does.

```elisp
;;; init.el --- Trev's Emacs config -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Trevor Richards
;;
;; Author: Trevor Richards <trev@trevdev.ca>
;; Maintainer: Trevor Richards <trev@trevdev.ca>
;; URL: https://github.com/trev-dev/emacs
;; Created: 2nd August, 2022
;; Version: 1.0.0
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
;; This program was generated using literate programming. See config.org for
;; details and documentation.
;;
;;; Code:
```


<a id="org8ec129b"></a>

## General Settings

Some general performance based improvements concerning large files, when to compile with the fancy new native-comp feature and reset the GC collection size after init.

```elisp
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(setq read-process-output-max (* 1024 1024))
(global-so-long-mode 1)
(setq comp-deferred-compilation t)
```

Clear out most of the GUI clutter, display relative line numbers, highlight the line I'm on, smaller left-only fringe, quick yes/no answers, some prog-mode QOL settings as well.

```elisp
;; Interface
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)
(setq display-line-numbers-type 'relative
      inhibit-startup-screen t
      initial-scratch-message ""
      history-length 25
      sentence-end-double-space nil
      visible-bell t)
(global-visual-line-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(fringe-mode '(4 . 0))
(defalias 'yes-or-no-p 'y-or-n-p)
```

Set up the dictionary and preferred browser.

```elisp
;; Spelling
(setq ispell-personal-dictionary "~/.config/emacs/personal-dict.pwd"
      ispell-program-name "aspell"
      ispell-dictionary "en"
      ispell-library-directory "~/.guix-home/profile/lib/aspell"
      ispell-alternate-dictionary (concat (getenv "HOME") "/Documents/wordlist"))
;; Browser
(setq browse-url-generic-program "/usr/bin/firefox")
```

I hate seeing project folders get all cluttered up. Let's move autosaves and backups somewhere else.

```elisp
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
```

Move custom variable settings to somewhere other that `init.el`.

```elisp
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
```

Start the pinentry service

```elisp
(pinentry-start)
```


<a id="org29eebb2"></a>

## Helper Functions

Many people configure their emacs with the help of macros such as `use-package`. I prefer to keep my init lower-level and simple. I have written some simple functions that will make configuration less repetative.

Using my own functions for the things I want or need keeps my overall package load smaller.

```elisp
(defun td/bind-keys (conses &optional mode-map)
  "Bind several keybinds using a list of `CONSES'.
Binds will be global unless the optional `MODE-MAP' is specified."
  (dolist (combo conses)
    (if (or (consp mode-map) (keymapp mode-map))
        (define-key mode-map (kbd (car combo)) (cdr combo))
      (if mode-map (warn "Optional %s `MODE-MAP' was invalid: %s" (type-of mode-map) mode-map))
      (global-set-key (kbd (car combo)) (cdr combo)))))

(defun td/add-hooks (modes func)
  "Set several hooks from a list of `CONSES'.
Adds '-hook' onto the end of the symbols for brevity."
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(defun td/auto-mode (modes)
  "Add the `MODES' to the `auto-mode-alist'."
  (dolist (mode modes)
    (add-to-list 'auto-mode-alist mode)))

(defun td/filter-nil (seq)
  "Filter out nil items from sequence `SEQ'."
  (seq-filter #'(lambda (item) item) seq))
```

Create a mode for mapping high priority keybinds early on.


<a id="orgd5229f7"></a>

### Priority Mode

Sometimes 3rd party packages like to take over my keyboard with their own keybinds. There are some keybinds that I prefer to always have access to without accidently triggering someone else's code first, then having to undo whatever that did, and use `M-x`.

With Priority mode, I am creating an "emulation layer". This is similar to what some popular modal editing packages do (such as evil-mode). It makes sure that when my `priority-mode` is active, the keybinds assigned to it will always take priority over other minor-mode bindings.

```elisp
(define-minor-mode priority-mode
  "A minor mode for short-listing keybindings.
This will prevent other modes form overriding keys that I would prefer to
see bound."
  :init-value nil
  :global t
  :keymap (make-sparse-keymap))
(add-to-list 'emulation-mode-map-alists `((priority-mode . ,priority-mode-map)))
(priority-mode)
```


<a id="org85e1607"></a>

## Keybinds

Change some of the built-in keybinds & bind some of the useful unbound functions.

```elisp
(defun td/forward-chunk ()
  (interactive)
  (next-line 20))

(defun td/backward-chunk ()
  (interactive)
  (previous-line 20))

(td/bind-keys '(("M-j" . join-line)
                ("M-n" . td/forward-chunk)
                ("M-p" . td/backward-chunk)))
```


<a id="org743aaa9"></a>

## Prog Mode

A few settings that are useful in programming buffers.

I am trying to respect the indent style of any file I come across, so I wrote some functions to help me with that.

```elisp
(defun td/toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (setq-local indent-tabs-mode (not indent-tabs-mode)))

(defun td/infer-indentation-style ()
  "Figure out whether or not we are indenting with tabs or spaces.
Set `indent-tabs-mode' accordingly."
  (let ((space-count (how-many "^  "))
        (tab-count (how-many "^\t")))
    (if (> space-count tab-count)
        (setq indent-tabs-mode nil))
    (if (> tab-count space-count)
        (setq indent-tabs-mode t))))
```

I need a setup hook that will trigger when prog-mode is activated.

```elisp
(defun td/prog-mode-settings ()
  "A general set-up hook for prog-mode."
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq whitespace-display-mappings '((tab-mark 9 [9474 9] [92 9])))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#636363")))))
  (setq-local fill-column 80)
  (setq-local show-trailing-whitespace t)
  (show-paren-mode t)
  (hs-minor-mode)
  (display-line-numbers-mode)
  (display-fill-column-indicator-mode)
  (electric-pair-local-mode)
  (td/infer-indentation-style)
  (whitespace-mode))
(add-hook 'prog-mode-hook 'td/prog-mode-settings)
```

I'd like to keep my tab style fixed at 2 spaces wherever possible. Specific programming modes can change this if they need to.

```elisp
(setq indent-tabs-mode nil)
(setq standard-indent 2)
(setq backward-delete-char-untabify-method 'hungry)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default electric-indent-inhibit t)
```


<a id="org6cb540c"></a>

## Package Configuration

In the following sections I will be configuring built-in packages as well as external packages via `package.el` and Quelpa.


<a id="org9c6ad8c"></a>

### Bootstrapping

I am using the built-in `package.el` for my package needs. I am using Quelpa for developing/contributing upstream, or installing some obscure package from source.


<a id="org073f7e8"></a>

#### Repositories

```elisp
(require 'package)
(dolist (repo '(("elpa" . "https://elpa.gnu.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives repo))
```


<a id="orge50e6f7"></a>

#### Maintaining selected packages

Here we will generage the `~/.emacs.d/package-list.el` file using the `noweb` feature that comes with org-mode.

```elisp
(defvar td/package-list
  (list
   'quelpa
   'all-the-icons
   'all-the-icons-dired
   'diminish
   'tangonov-theme
   'avy
   'ctrlf
   'cape
   'corfu-terminal
   'consult
   'consult-flycheck
   'corfu
   'pcmpl-args
   'fussy
   'kind-icon
   'marginalia
   'tempel
   'vertico
   'dashboard
   'diff-hl
   'elfeed
   'elfeed-org
   'capf-autosuggest
   'eshell-syntax-highlighting
   'expand-region
   'god-mode
   'goggles
   'magit
   'multiple-cursors
   'org-alert
   'org-chef
   'ox-gfm
   'ox-hugo
   'org-present
   'org-roam
   'org-roam-ui
   'ledger-mode
   'notmuch
   'org-mime
   'org-contacts
   'password-store
   'rg
   'visual-fill-column
   'which-key
   'clojure-mode
   'cider
   'sly
   'eglot
   'eldoc-box
   'emmet-mode
   'go-mode
   'lua-mode
   'markdown-mode
   'nim-mode
   'paredit
   'php-mode
   'prettier-js
   'pyvenv
   'rainbow-delimiters
   'rainbow-mode
   'inf-ruby
   'rust-mode
   'geiser-guile
   'tree-sitter
   'tree-sitter-langs
   'typescript-mode
   'web-mode
   'yaml-mode)
  "Packages that are defined in init.el and are meant to be used.
If `package-autoremove' wants to delete any of these, something is wrong.")

(dolist (pkg td/package-list)
  (unless (package-installed-p pkg)
    (unless package-archive-contents (package-refresh-contents))
    (package-install pkg t)))

(defun td/save-package-list ()
  "Customize `package-selected-packages' with `td/package-list'."
  (customize-save-variable 'package-selected-packages td/package-list))

(add-hook 'after-init-hook #'td/save-package-list)
```


<a id="org6ab3085"></a>

#### Quelpa

Bootstrap Quelpa if it is missing, then define a macro for a more intuitive way to install missing packages from remotes.

```elisp
quelpa
```

```elisp
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents   "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(setq quelpa-update-melpa-p nil)

(defmacro quelpa-get (pkg &rest method)
  "Use quelpa to retrieve some external `PKG', pass on `METHOD' to quelpa.
This should improve init by not looking for things we already have."
  `(unless (package-installed-p ',pkg)
     (quelpa '(,pkg ,@method))))
```


<a id="orga25570a"></a>

### Look & Feel


<a id="org65b1e51"></a>

#### All The Icons

```elisp
all-the-icons
```

```elisp
(require 'all-the-icons)
```


<a id="org58af203"></a>

#### Dired

```elisp
all-the-icons-dired
```

```elisp
(with-eval-after-load 'all-the-icons
  (setq all-the-icons-dired-monochrome nil)
  (add-hook
   'dired-mode-hook #'(lambda ()
                        (when (display-graphic-p)
                          (all-the-icons-dired-mode))
                        (dired-hide-details-mode))))
```


<a id="org4e23164"></a>

#### Diminish

Output from the `minor-mode-alist`. Due to how lazy-loading works, we want to make sure we have diminish early on.

```elisp
diminish
```

```elisp
(defun tdm/diminish-lsp-lighter ()
  "Display the LSP status in the `mode-line-modes'."
  (let* ((lsp-up lsp--buffer-workspaces)
         (color (if lsp-up '(:inherit success :weight bold)
                  '(:inherit warning :weight bold))))
    `(:propertize " LSP" face ,color)))

(defvar tdm/diminish-god-lighter
  '(:propertize
    " God" face
    (:inherit warning :weight bold))
  "Display god-mode state in the `mode-line-modes'.")

(dolist (mode '(("company" 'company-mode)
                ("hideshow" 'hs-minor-mode)
                ("undo-tree" 'undo-tree-mode)
                ("whitespace" 'whitespace-mode)
                ("yasnippet" 'yas-minor-mode)
                ("which-key" 'which-key-mode)
                ("org-indent" 'org-indent-mode)
                ("simple" 'visual-line-mode)
                ("eldoc" 'eldoc-mode)
                ("flymake" 'flymake-mode)
                ("flycheck" 'flycheck-mode)
                ("lsp-mode" 'lsp-mode '(:eval (tdm/diminish-lsp-lighter)))
                ("tree-sitter" 'tree-sitter-mode "TS")
                ("god-mode" 'god-local-mode tdm/diminish-god-lighter)
                ("beacon" 'beacon-mode)
                ("evil-goggles" 'evil-goggles-mode)
                ("evil-commentary" 'evil-commentary-mode)
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
```


<a id="orgce1c43c"></a>

#### Custom Theme

```elisp
tangonov-theme
```

```elisp
(load-theme 'tangonov t)
```


<a id="org258b65a"></a>

#### Font Setup

```elisp
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(add-to-list 'default-frame-alist '(font . "Hack 12"))
```


<a id="org98dc871"></a>

#### Modeline

```elisp
(defvar tdm/git-cached-status nil)
(defvar tdm/git--last-update nil)

(defun tdm/git-dirty? ()
  "Return t if local repository is dirty."
  (if (and
       tdm/git--last-update
       (< (float-time
           (time-subtract (current-time) tdm/git--last-update))
          1.0))
      tdm/git-cached-status
    (setq tdm/git--last-update (current-time))
    (setq tdm/git-cached-status
          (with-temp-buffer
            (vc-git-command t 0 nil "status" "--porcelain")
            (> (buffer-size) 0)))))

(defun tdm/vc ()
  "Get the git status for the current buffer."
  (when-let (vc vc-mode)
    (let* ((dirty (tdm/git-dirty?))
           (icon (if dirty "  ±" "  ✔"))
           (color (if dirty '(:foreground "#82AAFF") 'success)))
      `(:propertize ,(concat icon " " (substring vc 5)) face ,color))))

(defcustom td/custom-project-name nil
  "A custom directory-local name for a project.el project."
  :type 'string)

(defun tdm/project()
  "Display the current project name, or path."
  (when (project-current)
    (concat (propertize (if (stringp td/custom-project-name)
                     td/custom-project-name
                   (file-name-nondirectory
                    (directory-file-name
                     (project-root (project-current)))))
                 'face 'success
                 'help-echo "Switch project"
                 'mouse-face '(:box 1)
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 #'project-switch-project))
            (propertize ":" 'face
                        '(:inherit font-lock-comment-face)))))

(defun tdm/modal-face (str base)
  (propertize str 'face
              `(:inherit ,base :weight bold :height 0.9)))

(defvar tdm/custom-meow-states `((normal . ,(tdm/modal-face
                                             "<N>" '(:foreground "#FFCA41")))
                                 (motion . ,(tdm/modal-face
                                             "<M>" '(:foreground "#82AAFF")))
                                 (keypad . ,(tdm/modal-face
                                             "<K>" '(:foreground "#89DDFF")))
                                 (insert . ,(tdm/modal-face
                                             "<I>" '(:foreground "#C792EA")))
                                 (beacon . ,(tdm/modal-face
                                             "<B>" '(:foreground "#FF7B85")))))

(defvar tdm/evil-states `((normal . ,(tdm/modal-face
                                      "<N>" '(:foreground "#FFCA41")))
                          (motion . ,(tdm/modal-face
                                      "<M>" '(:foreground "#82AAFF")))
                          (operator . ,(tdm/modal-face
                                        "<O>" '(:foreground "#89DDFF")))
                          (insert . ,(tdm/modal-face
                                      "<I>" '(:foreground "#ABDC88")))
                          (visual . ,(tdm/modal-face
                                       "<V>" '(:foreground "#FF996B")))
                          (replace . ,(tdm/modal-face
                                       "<R>" '(:foreground "#FF7B85")))
                          (emacs . ,(tdm/modal-face
                                     "<E>" '(:foreground "#C792EA")))))

(defun tdm/meow-state ()
  "Retrieve the meow-state for the mode-line."
  (when (featurep 'meow)
    (concat (alist-get (meow--current-state) tdm/custom-meow-states)
            " ")))

(defun tdm/evil-state ()
  "Get the evil state for the mode-line."
  (when (featurep 'evil)
    (concat (alist-get evil-state tdm/evil-states)
            " ")))

(defun tdm/god-state ()
  "Get the god-mode state for the mode-line."
  (when (featurep 'god-mode)
    (format "%s " (if god-local-mode
                      (tdm/modal-face "<G>" '(:foreground "#FFCA41"))
                    (tdm/modal-face "<E>" '(:foreground "#C792EA"))))))

(defun tdm/status-flag (on face)
  "Produce a status flag based on some `PRED'icate test and give it a `FACE'."
  (format "%s" (if on
                   (propertize "▰" 'face `(:inherit ,face :weight bold))
                 "-")))

(defun tdm/buffer-position ()
  "Display the mode-line buffer position."
  (concat "  %l:%c"
        (propertize " %p" 'face '(:inherit font-lock-comment-face))))

(defun tdm/buffer-size ()
  "Display the mode-line buffer size."
  (format "%s" (propertize " (%I)" 'face '(:inherit font-lock-comment-face))))

(defun tdm/split-format (left right)
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

(defun tdm/flycheck ()
  "Get the flycheck status for the buffer, if LSP mode is not doing so."
  (when (and (bound-and-true-p flycheck-mode)
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

(defun tdm/flymake ()
  "Display the flymake status for the buffer."
  (when (bound-and-true-p flymake-mode)
    " "
    flymake-mode-line-title
    flymake-mode-line-exception
    flymake-mode-line-counters))

(defun tdm/misc ()
  "Get a trimmed version of the `mode-line-misc-info'."
  (let ((info (format-mode-line mode-line-misc-info)))
    (unless (string= info "")
      (list "  " (string-trim info)))))

(defun tdm/macro-indicator ()
  "Indicate when a macro is being recorded in the mode-line."
  (when defining-kbd-macro
    (format "%s" (propertize
                  "λ" 'face '(:inherit bold :foreground "#C792EA")))))

(setq-default mode-line-format
              '((:eval
                 (tdm/split-format
                  ;; Left
                  (format-mode-line
                   '(" "
                     (:eval (tdm/evil-state))
                     (:eval (tdm/status-flag buffer-read-only 'error))
                     (:eval (tdm/status-flag (buffer-modified-p) 'warning))
                     (:eval (if (not (eq
                                      (format-mode-line mode-line-client)
                                      ""))
                                (tdm/status-flag t '(:foreground "#C792EA"))
                              ""))
                     " "
                     (:eval (tdm/project))
                     mode-line-buffer-identification
                     (:eval (tdm/buffer-size))
                     (:eval (tdm/buffer-position))))
                  ;; Right
                  (format-mode-line
                   '((:eval (tdm/macro-indicator))
                     (:eval (tdm/vc))
                     (:eval (tdm/misc))
                     "  "
                     mode-line-modes))))))
```


<a id="org9741e49"></a>

### Utility Packages

Packages that extend and augment emacs in a general way


<a id="org6fb1fd4"></a>

#### Avy

```elisp
avy
```

```elisp
(define-key priority-mode-map (kbd "C-:") #'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "C-:") #'avy-isearch)
(avy-setup-default)
```


<a id="org36c8af6"></a>

#### CTRLF

CTRLF greatly enhances isearch.

```elisp
ctrlf
```

```elisp
(ctrlf-mode 1)
```


<a id="orgf8f22c5"></a>

#### Completions

A combination of packages to enhance completions.


<a id="org1c4f19e"></a>

##### Cape

Add completion at point functions for things like Corfu

```elisp
cape
```

```elisp
(defun td/don-local-cape (comps &optional no-extend)
  "Create a hook function to set local capfs to include `COMPS'.
If `NO-EXTEND' is non-nil, the global capfs will be discarded."
  `(lambda ()
     (setq-local completion-at-point-functions
                 (if ,no-extend
                     ',comps
                   ',(cl-union comps completion-at-point-functions)))))

(setq cape-dict-file (concat
                      (getenv "HOME")
                      "/Documents/wordlist"))
(defvar td/capes
  (let ((map (make-sparse-keymap)))
    (td/bind-keys '(("p" . completion-at-point)
                    ("t" . complete-tag)
                    ("d" . cape-dabbrev)
                    ("h" . cape-history)
                    ("f" . cape-file)
                    ("k" . cape-keyword)
                    ("s" . cape-symbol)
                    ("a" . cape-abbrev)
                    ("i" . cape-ispell)
                    ("l" . cape-line)
                    ("w" . cape-dict)
                    ("&" . cape-sgml)
                    ("t" . tempel-expand)
                    ("r" . cape-rfc1345)) map)
    map) "Keymap for the various cape completion functions. \\{td/capes}")
(fset 'td/capes td/capes)

  (global-set-key (kbd "C-c M-p") 'td/capes)

(add-hook 'prog-mode-hook (td/don-local-cape (list (cape-super-capf
                                                    #'cape-keyword
                                                    #'tempel-expand)
                                                   #'cape-file
                                                   #'cape-dabbrev)))

(add-hook 'emacs-lisp-mode-hook (td/don-local-cape
                                 (list (cape-super-capf
                                        #'cape-symbol
                                        #'cape-keyword
                                        #'tempel-expand)
                                       #'cape-file
                                       #'cape-dabbrev) t))

(add-hook 'geiser-mode-hook (td/don-local-cape
                             (list #'geiser-capf--for-module
                                   #'geiser-capf--for-symbol
                                   #'geiser-capf--for-filename
                                   #'tempel-expand
                                   #'cape-file
                                   #'cape-dabbrev) t))

(add-hook 'text-mode-hook (td/don-local-cape (list #'tempel-expand #'cape-dict)))
```

```elisp
corfu-terminal
```

```elisp
(unless (display-graphic-p)
  (corfu-terminal-mode t))
```


<a id="org41636f3"></a>

##### Consult

I am currently giving consult a try as my completion-at-point solution, amongst many other better ways to reference things in Emacs.

```elisp
consult
consult-flycheck
```

```elisp
  (require 'consult)

  (setq register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (td/bind-keys '(("C-c h" . consult-history)
                  ("C-c M-x" . consult-mode-command)
                  ;; ("C-c k" . consult-kmacro)
                  ;; C-x bindings (ctl-x-map)
                  ("C-x M-:" . consult-complex-command)
                  ("C-x C-b" . consult-buffer)

                  ("C-x r b" . consult-bookmark)
                  ;; Custom M-# bindings for fast register access
                  ("M-#" . consult-register-load)
                  ("M-'" . consult-register-store)
                  ("C-M-#" . consult-register)
                  ("M-g f" . consult-flymake) ; or flymake?
                  ("M-g o" . consult-outline)
                  ("M-g m" . consult-mark)
                  ("M-g k" . consult-global-mark)
                  ("M-g i" . consult-imenu)
                  ("M-g I" . consult-imenu-multi)
                  ;; M-s bindings (search-map)
                  ("M-s d" . consult-find)
                  ("M-s D" . consult-locate)
                  ("M-s g" . consult-grep)
                  ("M-s G" . consult-git-grep)
                  ("M-s r" . consult-ripgrep)
                  ("M-s l" . consult-line)
                  ("M-s L" . consult-line-multi)
                  ("M-s m" . consult-multi-occur)
                  ("M-s k" . consult-keep-lines)
                  ("M-s u" . consult-focus-lines)
                  ;; Isearch integration
                  ("M-s e" . consult-isearch-history)))

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "M-g o") #'consult-org-heading))

  (define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<"
        consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
```


<a id="org561f111"></a>

##### Corfu

Drop-down style completions & related packages. I use Corfu everywhere, hence adding pcmpl-args, which is supposed to enhance eshell completions.

```elisp
corfu
pcmpl-args
```

```elisp
(setq tab-always-indent 'complete)

(setq corfu-auto t
      corfu-quit-no-match t)
(global-corfu-mode)

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (corfu-mode 1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))
(advice-add #'corfu-insert :after #'corfu-send-shell)

;; Silence the pcomplete capf, no errors or messages!
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(require 'pcmpl-args)
```


<a id="orge94f252"></a>

##### Fussy

A pretty good fuzzy completion style.

```elisp
fussy
```

```elisp
(add-to-list 'completion-styles 'fussy t)
(setq completion-category-defaults nil
      completion-category-overrides nil)
```


<a id="org6026003"></a>

##### Kind-Icon

```elisp
kind-icon
```

```elisp
(with-eval-after-load 'corfu
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
```


<a id="org0b4a8bb"></a>

##### Marginalia

Better descriptions of symbols in the minibuffer.

```elisp
marginalia
```

```elisp
(marginalia-mode)
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
```


<a id="org72754b3"></a>

##### Savehist

Save history for Vertico to look at later.

```elisp
(savehist-mode)
```


<a id="org87e60db"></a>

##### TempEl

Snippet completions written in elisp.

Note to self: This is intertwined with [cape](#org1c4f19e).

```elisp
tempel
```

```elisp
(global-set-key (kbd "C-c M-t") #'tempel-insert)
```


<a id="org612190b"></a>

##### Vertico

Mini-buffer completions back-end.

```elisp
vertico
```

```elisp
(with-eval-after-load 'consult
  (vertico-mode)
  (setq enable-recursive-minibuffers t))
```


<a id="org7f82336"></a>

#### Dashboard

```elisp
dashboard
```

```elisp
(setq dashboard-startup-banner 'logo
      dashboard-projects-backend 'project-el
      dashboard-items '((projects . 5)
                        (recents . 5)
                        (agenda . 5)
                        (bookmarks . 5))
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-center-content t
      dashboard-set-init-info t)
(when (daemonp)
  (setq initial-buffer-choice
        (lambda () (get-buffer "*dashboard*"))))
(dashboard-setup-startup-hook)
```


<a id="orgf779f48"></a>

#### Diff-hl

Show me the diffs in the fringe!

```elisp
diff-hl
```

```elisp
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
```


<a id="org06d85c9"></a>

#### Elfeed

RSS Reader :D

```elisp
elfeed
elfeed-org
```

```elisp
(global-set-key (kbd "<f6>") #'elfeed)

(with-eval-after-load 'elfeed
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/Org/elfeed.org")))
```


<a id="orgda59bcf"></a>

#### Surround

My attempt at writing a quick replacement for "vim surround". There are better solutions out there, but when they're unpredictable, I don't want to figure out why.

This is a "dumb" solution. It just seeks backward for the start of a pair, then matches the surround with a forward sexp. If it's called with a neg-arg (eg: `(surround -1)`) it will scan forward first and look back. If the point is not inside the bounds of a resulting backward scan, we fall back to forward.

Ideally if the point is not inside the bounds of a found sexp, I should be scanning recursively in the same direction until it does. Maybe I'll implement this later.

When a pair is not in `surround-pairs`, it will fall-back to symmetrical pairs (a pair of the same char). When this happens, scanning forward, or backward, makes no difference.

It currently doesn't care about the scope of the scan, either, and doesn't care if the backward or forward sexp is 100 lines elsewhere.

I should refine this.

```elisp
(defvar surround-pairs '(("{" . "}")
                         ("(" . ")")
                         ("[" . "]")
                         ("<" . ">"))
  "A list of asymmetric pairs for `surround' to respect.")

(defun surround--seek-outer-boundary (start left pair count)
  "Seek out the boundary of an outside `PAIR' from the `START'.
If `LEFT' is non-nil, seek left. Otherwise, seek right."
  (let* ((search (if left #'search-backward #'search-forward))
         (ch-match (if left (car pair) (cdr pair)))
         (ch-skip  (if left (cdr pair) (car pair)))
         (sym (eq ch-skip ch-match))
         (case-fold-search nil))
    (save-excursion
      (if sym
          (apply search (list ch-match nil t count))
        (let* ((match (apply search (list ch-match nil t count)))
               (mid (push-mark start t t))
               (imbalance (count-matches (regexp-quote ch-skip)
                                         (region-beginning)
                                         (region-end)))
               (mcount (count-matches (regexp-quote ch-match)
                                      (region-beginning)
                                      (region-end))))
          (if (and (>= imbalance mcount) match)
              (surround--seek-outer-boundary start left pair
                                             (+ (- imbalance mcount) 1))
            (deactivate-mark)
            match))))))

(defun surround--seek-bounds (pair)
  "Find the bounds of a surrounding `PAIR' around the point."
  (let ((bounds (cons (surround--seek-outer-boundary (point) t pair 1)
                      (surround--seek-outer-boundary (point) nil pair 1))))
    (if (and (car bounds) (cdr bounds))
        bounds
      (user-error (format "No surrounding pair: %s" pair)))))

(defun surround--add-pair (bounds pair)
  "Add an arbitrary surrounding `PAIR' of chars to a `BOUNDS'."
  (save-excursion
    (goto-char (car bounds))
    (insert (car pair))
    (goto-char (+ (cdr bounds) 1))
    (insert (cdr pair))))

(defun surround--delete-pair (bounds)
  "Delete a surrounding pair outside the `BOUNDS' a range of positions."
  (save-excursion
    (goto-char (- (cdr bounds) 1))
    (delete-char 1)
    (goto-char (car bounds))
    (delete-char 1)))

(defun surround--change-pair (bounds)
  "Swap out an exisiting `PAIR' outside of `BOUNDS'."
  (let* ((to-what (char-to-string (read-char (message "To new pair: "))))
         (new-pair (or (assoc to-what surround-pairs)
                       (rassoc to-what surround-pairs)
                       (cons to-what to-what))))
    (surround--delete-pair bounds)
    (surround--add-pair (cons (car bounds) (- (cdr bounds) )) new-pair)))

(defun surround (neg)
  "Add surrounding pairs to a region, or change/delete an existing pair.
Inspired by vim-surround. Scans forward. Use `NEG'-arg to scan backward
for pair."
  (interactive "p")
  (let* ((reverse (< neg 0))
         (case-fold-search nil)
         (method (if (and (region-active-p)
                          (not (eq (region-beginning) (region-end))))
                     ?a
                   (read-char-choice "(c)hange or (d)elete pair? " '(?c ?d))))
         (target (char-to-string (read-char (message "Pair:"))))
         (pair (or (assoc target surround-pairs)
                   (rassoc target surround-pairs)
                   (cons target target)))
         (bounds (or (and
                      (region-active-p)
                      (car (region-bounds)))
                     (surround--seek-bounds pair))))
    (cond ((eq method ?a) (surround--add-pair bounds pair))
          ((eq method ?c) (surround--change-pair bounds))
          ((eq method ?d) (surround--delete-pair bounds)))))

(global-set-key (kbd "C-S-s") #'surround)
```


<a id="org0fdb0ed"></a>

#### ERC

```elisp
(setq erc-autojoin-channels-alist
      '(("Libera.Chat" "#emacs" "#guix" "#systemcrafters" "#stumpwm")))

(defun td/launch-erc ()
  (interactive)
  (erc-tls :server "irc.libera.chat"
                             :port 7000
                             :nick "trevdev"
                             :password (password-store-get
                                        "Biz/libera.chat")))
```


<a id="org3384f1b"></a>

#### Eshell

```elisp
capf-autosuggest
eshell-syntax-highlighting
```

```elisp
(defun td/eshell-extras ()
  "Start extra features for eshell-mode"
  (eshell-syntax-highlighting-mode)
  (capf-autosuggest-mode))

(add-hook 'eshell-mode-hook #'td/eshell-extras)
```


<a id="orgdd33a0a"></a>

#### Expand Region

It just makes selecting text between sexps easy.

```elisp
expand-region
```

```elisp
(require 'expand-region)
(td/bind-keys '(("C-=" . er/expand-region)))
(defvar er/keymap
  (let ((map (make-sparse-keymap "er/objects")))
    (td/bind-keys '(("w"   . er/mark-word)
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
(define-key priority-mode-map (kbd "C-,") er/keymap)
```


<a id="orge743755"></a>

#### God Mode

God mode is an amazing package. It automatically translates key-chords into single-key bindings and toggled modifiers.

Because it has its own keymap, I can add utility functions to god-mode. This turns it into sort of a pseudo-modal editing mode. However, unlike other modal packages, it does not require as much key re-binding, thanks to key-chord translation.

```elisp
god-mode
```


<a id="org1857d14"></a>

##### Functions

These functions enhance editing while allowing me to "drop out" of god-mode in useful ways.

```elisp
(defun god/eol-insert ()
  "Move the cursor to the end-of-line and exit god mode."
  (interactive)
  (end-of-line)
  (god-local-mode -1))

(defun god/boi-insert ()
  "Move the cursor `back-to-indentation' and exit god mode."
  (interactive)
  (back-to-indentation)
  (god-local-mode -1))

(defun god/forward-insert ()
  "Move the cursor over one char and exit god mode."
  (interactive)
  (forward-char)
  (god-local-mode -1))

(defun god/change ()
  "Kill char/region and exit god mode."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (zap-to-char 1 (char-after)))
  (god-local-mode -1))

(defun god/backward-symbol (num)
  "Move backward `NUM' symbols."
  (interactive "^p")
  (forward-symbol (- 0 (or (when (natnump num) num) 1))))

(defun god/open-above ()
  "Open a new line above the current line, put the point there."
  (interactive)
  (beginning-of-line)
  (split-line)
  (god-local-mode -1))

(defun god/open-below ()
  "Open a new line below the current line, put the point there."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (god-local-mode -1))

(defun god/pull-line ()
  "Pull a line up from below the currnet line and join them."
  (interactive)
  (save-excursion
    (next-line)
    (join-line)))
```


<a id="orgd084905"></a>

##### Insert Ahead

I want some way to intuitively leave god mode one character over from where I scanned to with seeking or moving forward and backward.

This comes in handy because sometimes words separated by non-word characters can put you in a spot where if you could move just one character "over", you could be right where you want to land without having to move a whole word/thing over the mark and back again.

```elisp
(defvar god/ahead-direction 1
  "A cached value of the presumed `god/insert-ahead' direction.")

(defun god/set-ahead-direction (&optional dir)
  "Set `god/ahead-direction'. If `DIR' is 1, it's forward.
A value of -1 is backward.'"
  (let ((direction (or dir 1)))
    (unless (= direction god/ahead-direction)
      (setq-local god/ahead-direction direction))))

(defun god/insert-ahead (&rest args)
  "Move the cursor in `god/ahead-direction' and exit `god-local-mode'."
  (interactive)
  (forward-char god/ahead-direction)
  (god-local-mode -1))

(add-hook 'god-local-mode-hook
          #'(lambda () (god/set-ahead-direction)))

(dolist (back-func '(backward-char
                     backward-word
                     god/backward-symbol
                     isearch-backward
                     isearch-backward-regexp
                     search-backward
                     search-backward-regexp))
  (advice-add back-func :after
              #'(lambda (&rest args) (god/set-ahead-direction -1))
              '((name . "god/set-ahead-backward"))))

(dolist (for-func '(forward-char
                    forward-word
                    forward-symbol
                    isearch-forward
                    isearch-forward-regexp
                    search-backward
                    search-forward-regexp))
  (advice-add for-func :after
              #'(lambda (&rest args) (god/set-ahead-direction))
              '((name . "god/set-ahead-forward"))))
```


<a id="org2462356"></a>

##### Org Mode Newline Advice

I would like to be able to perform special org-mode functions such as `org-meta-return` and `org-insert-todo-heading` and have `god-local-mode` turn off automatically.

```elisp
(advice-add 'org-meta-return :after
            #'(lambda (&rest args) (god-local-mode -1))
            '((name . "god/insert-after-org-meta-return")))

(advice-add 'org-insert-todo-heading :after
            #'(lambda (&rest args) (god-local-mode -1))
            '((name . "god/insert-after-org-new-heading")))
```


<a id="org4d72ca6"></a>

##### Seeking Characters

I envied Vim's ability to use `f` or `t` to quickly jump to, or just past a char target. I wrote my own solution. You can even repeat the last seek, or throw it into reverse with a negative argument.

```elisp
(defvar god/previous-seek-motion nil
  "The previous until/find motion performed by god-mode.")

(defun god/seek (n &optional until-p repeat-ch)
  "Move the cursor forward, or backword to the nearest char in `N' direction.
Can be called with a `REPEAT-CH' to automatically seek for or `UNTIL-P' a char."
  (interactive "p")
  (let* ((case-fold-search nil)
         (ch (or repeat-ch
                 (read-char
                  (message "Seek%s(%d):" (if until-p "-Until" "") n))))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (fix-pos (if until-p (if (< n 0) 1 -1) 0))
         end)
    (save-excursion
      (if (< n 0) (forward-char -1) (forward-char 1))
      (setq end (search-forward ch-str nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (setq god/previous-seek-motion `(god/seek ,n ,until-p ,ch))
      (god/set-ahead-direction n)
      (goto-char (+ end (if until-p fix-pos 0))))))

(defun god/seek-until (neg-arg &optional repeat-ch)
  "Seek up to but not including a char.
Direction can be modified with a `NEG-ARG'. Can be repeated with a `REPEAT-CH'."
  (interactive "p")
  (god/seek neg-arg t repeat-ch))

(defun god/repeat-seek (reverse)
  "Repeat the `god/previous-seek-motion'.
Apply a neg-arg to go in `REVERSE'"
  (interactive "p")
  (when god/previous-seek-motion
    (let ((func (car god/previous-seek-motion))
          (num (cadr god/previous-seek-motion))
          (until (caddr god/previous-seek-motion))
          (ch (cadddr god/previous-seek-motion)))
      (funcall func (if (< reverse 0) (* num -1) num) until ch))))
```


<a id="orgac39169"></a>

##### Cursor Indicator

I like having a thick bar for "emacs mode" and a box for god-mode.

```elisp
(setq cursor-type '(bar . 4))

(defun god/cursor-toggle ()
  "Toggle the cursor between a box and bar while in or out of `god-mode'."
  (setq cursor-type (if (bound-and-true-p god-local-mode)
                        'box
                      '(bar . 4))))
```


<a id="org73dc584"></a>

##### Keybindings

Declare key-bindings to be applied in the next section.

```elisp
(defvar god/keybinds '(("h" . backward-char)
                       ("j" . next-line)
                       ("k" . previous-line)
                       ("l" . forward-char)
                       (";" . god/repeat-seek)
                       ("A" . god/boi-insert)
                       ("B" . god/backward-symbol)
                       ("b" . backward-word)
                       ("C" . god/change)
                       ("D" . delete-backward-char)
                       ("E" . god/eol-insert)
                       ("F" . forward-symbol)
                       ("g" . avy-goto-char-timer)
                       ("f" . forward-word)
                       ("u" . undo)
                       ("U" . undo-redo)
                       ("I" . god/insert-ahead)
                       ("i" . god-local-mode)
                       ("J" . god/pull-line)
                       ("O" . god/open-above)
                       ("o" . god/open-below)
                       ("w" . td/windmove-map)
                       ("T" . god/seek)
                       ("t" . god/seek-until)
                       ("P" . td/backward-chunk)
                       ("N" . td/forward-chunk)
                       ("q" . quit-window)
                       ("z" . repeat)
                       ("," . er/keymap)))
```


<a id="orgba3bf68"></a>

##### Apply & Finish Setup

I want god mode to be available to me everywhere. To do this, `god-exempt-major-modes` needs to be unset before loading `god-mode`.

I would prefer to keep god mode on, or off, on a buffer-to-buffer basis. I use `god-local-mode` for this.

God has no intermediary mode for non-editing buffers. I feel like it's better to have to turn it on explicitly for quicker navigation or firing off commands.

```elisp
(setq god-mode-enable-function-key-translation nil
      god-exempt-major-modes '(vterm-mode)
      god-exempt-predicates nil
      god-mode-alist '((nil . "C-")
                       ("m" . "M-")
                       ("M" . "C-M-")))

(require 'god-mode)
(require 'god-mode-isearch)

(global-set-key (kbd "<escape>") #'god-mode-all)
(define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

(add-to-list 'emulation-mode-map-alists
             `((god-local-mode . ,god-local-mode-map)))

(td/bind-keys god/keybinds god-local-mode-map)

(god/cursor-toggle)

(with-eval-after-load 'which-key
  (which-key-enable-god-mode-support))

(add-hook 'post-command-hook #'god/cursor-toggle)
(add-hook 'god-local-mode-hook #'corfu-quit)
```


<a id="orgd3abddd"></a>

#### Goggles

Extra feedback for text changes.

```elisp
goggles
```

```elisp
(td/add-hooks '(text-mode prog-mode) #'goggles-mode)
(setq-default goggles-pulse t)
```


<a id="org4c11dfa"></a>

#### Magit

Magit is one of the biggest reasons why I fell in love with emacs. It's the best keyboard driven "TUI" abstraction of the git command line anywere, period. Better than Fugitive by far. Sorry, Tim Pope.

```elisp
magit
```

```elisp
(global-set-key (kbd "C-c g") #'magit-status)
```


<a id="orgddb3094"></a>

#### Multiple Cursors

```elisp
multiple-cursors
```

```elisp
(td/bind-keys '(("C-S-l"   . mc/edit-lines)
                ("C->"     . mc/mark-next-like-this)
                ("C-<"     . mc/mark-previous-like-this)
                ("C-M->"   . mc/skip-to-next-like-this)
                ("C-M-<"   . mc/skip-to-previous-like-this)
                ("C-c C-?" . mc/mark-all-like-this-dwim)
                ("C-c C-/" . mc/mark-all-in-region)
                ("C-M-n"   . mc/insert-numbers)
                ("C-M-a"   . mc/insert-letters))
              priority-mode-map)
```


<a id="orgde09f37"></a>

#### Org

The greatest part of using Emacs is org-mode. It handles my agenda, my todo list, helps me prioritize tasks, track time and invoice clients.


<a id="org75f75ed"></a>

##### Key Variables

I am using tags to help sort contexts within my agenda. Some people use categories for that. I technically do that, too, but I also use separate files. Filenames are categories by default, so there is less to configure when you use separate files.

```elisp
(defvar td/tag-list
  '((:startgroup)
    ("@home" . ?H)
    ("@work" . ?W)
    (:endgroup)
    ("foss"  . ?f)
    ("gurps" . ?g)
    ("idea"  . ?i))
  "The tags for org headlines.")
```

Next are my TODO key words. They are meant to be used as such:

-   `TODO` A generic task or actionable thing.
-   `NEXT` A planned task, something I am setting my mind to until it is done. There should be very few of these types of tasks so that I am setting achievable goals
-   `WAIT` The task that is held up by some pre-requesite or external factor
-   `LOW` The task is a "maybe/someday" task. I'd like to see it done, but it's not a priority right now.
-   `DONE` The task is completed
-   `PASS` The task has been "passed along" or "delegated" to someone else. Considered 'done', just not by myself
-   `CANC` The task has been cancelled or ended before completion

```elisp
(defvar td/todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "LOW(l)"
              "|" "DONE(d!)" "PASS(p@)" "CANC(k@)"))
  "A sequence of keywords for Org headlines.")
```

My org agenda commands & stuck projects. Currently a work in progress! I am reading David Allen's "[Getting Things Done](https://gettingthingsdone.com/)." I am attempting to shape my agenda to suit that system.

```elisp
(defvar td/org-agenda-commands
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
              '(org-agenda-skip-entry-if 'scheduled 'deadline))))
      )
     )
    ("l" "Backburner of low priority tasks"
     ((todo "LOW"
           ((org-agenda-overriding-header "Someday/Maybe"))))
     )
    )
  "Custom commands for Org Agenda.")
```

Capture templates! These help me collect information into Org files. Currently I only have 2 cookbook capture methods that are meant to be used with org-chef. See [extensions](#orgd8318e6) for how I extend org-mode.

```elisp
(defvar td/capture-templates
  '(("t" "Todo" entry (file+headline "~/Org/agenda.org" "Inbox")
     "* TODO %^{Title: }\n:PROPERTIES:\n:date: %U\n:END:\n%?"
     :empty-lines 1)
    ("c" "Contact" entry (file+headline "~/Org/contacts.org" "Other")
     "* %^{Name: }\n:PROPERTIES:\n:email: %?\n:END:"
     :empty-lines 1))
  "Base org-capture-templates.")

(global-set-key (kbd "C-c M-a") #'org-capture)
```

I usually stick to monospace sized fonts with the exception of Org files. I like the first 3 levels to be slightly larger than the rest, and progressively smaller. This helps me create a sense of urgency at the lower-level headers and it also improves readability.


<a id="org5b841eb"></a>

##### Functions

Some fairly self-explanatory utility functions.

```elisp
(defvar td/org-scale-levels-enable nil
  "Whether or levels are scaled.")

(defun td/org-scale-levels-toggle (&optional enable)
  "Enlarge org levels for more readability."
  (interactive)
  (let ((scaled (or enable (not td/org-scale-levels-enable))))
    (dolist (face '((org-level-1 . (if scaled 1.2 1.0))
                    (org-level-2 . (if scaled 1.1 1.0))
                    (org-level-3 . (if scaled 1.05 1.0))))
      (set-face-attribute (car face) nil :weight 'semi-bold :height (eval (cdr face))))
    (setq td/org-scale-levels-enable scaled)))

(defun td/org-hook ()
  "Do some stuff on org mode startup."
  (org-clock-persistence-insinuate)
  (org-indent-mode)
  (setq-local line-spacing 0.1))

(defun td/org-append-templates (templates)
  (setq org-capture-templates (append org-capture-templates templates)))
```


<a id="org7cb8bb5"></a>

##### Apply Configuration

```elisp
(add-hook 'org-mode-hook #'td/org-hook)
(global-set-key (kbd "C-c a") #'org-agenda)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t") #'org-table-export))

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
      org-agenda-files '("~/Org")
      org-tag-alist td/tag-list
      org-todo-keywords td/todo-keywords
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes t
      org-refile-targets '((org-agenda-files :maxlevel . 4))
      org-clock-sound "~/.config/emacs/inspectorj_bell.wav"
      org-timer-default-timer "25"
      org-agenda-custom-commands td/org-agenda-commands
      org-stuck-projects '("/PROJ-DONE" ("TODO" "NEXT") nil "- \\[ \\]")
      org-capture-templates td/capture-templates
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t)

(add-to-list 'display-buffer-alist '("\\*Org Agenda*\\*"
                                     (display-buffer-in-direction)
                                     (direction . right)
                                     (window-width . 0.50)
                                     (window-height . fit-window-to-buffer)))
```


<a id="orgd8318e6"></a>

##### Extending Org Mode

Extending org-mode with some interesting packages.


###### org-alert

Libnotify alerts for Agenda alerts.

```elisp
org-alert
```

```elisp
(with-eval-after-load 'org
  (require 'org-alert)
  (setq alert-default-style 'libnotify
        org-alert-interval 7200
        org-alert-notify-cutoff 60
        org-alert-notification-title "Org Agenda")
  (org-alert-enable))
```


###### org-chef

[Org-chef](https://github.com/Chobbes/org-chef) is a must have if you enjoy cooking. You can just use `M-x org-chef-insert-recipe` in whatever cookbook file, or the capture templates.

```elisp
org-chef
```

```elisp
(td/org-append-templates
 '(("r" "Recipe" entry (file "~/Projects/cookbook/src/cookbook.org")
    "%(org-chef-get-recipe-from-url)"
    :empty-lines 1)
   ("m" "Manual Cookbook" entry
    (file "~/Projects/cookbook/src/cookbook.org")
    (eval (concat "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n"
            "  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n"
            "  :END:\n** Ingredients\n   %?\n** Directions\n\n")))))
```


###### ox-gfm

Get access to Github Flavored Markdown

```elisp
ox-gfm
```

```elisp
(with-eval-after-load 'ox
  (require 'ox-gfm))
```


###### ox-hugo

I like org-publish, but there are some files (like my cookbook) that I would like to keep in one document, as it is a capture file, and be able to easily publish it into a list of "posts".

```elisp
ox-hugo
```

```elisp
(with-eval-after-load 'ox
  (require 'ox-hugo))
```


###### org-present

A tiny package for presenting with org-mode.

```elisp
org-present
```

```elisp
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
```


###### org-roam

Org roam is an incredible thought capture system, inspired by roam research. I'm not sure this one's for me, but I am giving it a try.

```elisp
org-roam
```

```elisp
(defvar td/roam-capture-templates
  '(("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+DATE: %U\n")
     :unnarrowed t)))

(defvar td/roam-capture-daily
  '(("d" "default" entry "* %<%I:%M %p>: %?"
     :target (file+head "%<%Y-%m-%d>.org"
                        "#+TITLE: %<%a, %b %d %Y>\n"))
    ("p" "Private" entry "* %<%I:%M %p>: %?"
     :target (file+head "%<%Y-%m-%d>.org.gpg"
                        "#+TITLE: %<%a, %b %d %Y>\n"))))

(defvar td/roam-display-template
  (concat "${title:*} "
          (propertize "${tags:28}" 'face 'org-tag)))

(td/bind-keys '(("C-c r t" . org-roam-buffer-toggle)
                ("C-c r f" . org-roam-node-find)
                ("C-c r i" . org-roam-node-insert)
                ("C-c r c" . org-roam-capture)
                ("C-c r d i" . org-roam-dailies-capture-today)
                ("C-c r d t" . org-roam-dailies-goto-today)
                ("C-c r d y" . org-roam-dailies-goto-yesterday)
                ("C-c r d d" . org-roam-dailies-goto-date)))

(setq org-roam-capture-templates td/roam-capture-templates
      org-roam-dailies-capture-templates td/roam-capture-daily
      org-roam-node-display-template td/roam-display-template
      org-roam-db-node-include-function
      (lambda ()
        (not (member "attach" (org-get-tags))))
      org-roam-directory (file-truename "~/Org/roam"))

(add-to-list 'display-buffer-alist '("\\*org-roam\\*"
                                     (display-buffer-in-direction)
                                     (direction . right)
                                     (window-width . 0.33)
                                     (window-height . fit-window-to-buffer)))
(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode))
```


###### org-roam-ui

A fancy, web-based user interface for reviewing your org-roam notes and how they connect to one-another.

```elisp
org-roam-ui
```

```elisp
(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)
```


<a id="org429f8cb"></a>

##### Custom Clock Table

I wanted a neat and tidy way to lay out the hours that I've worked, vs how much effort they should have taken & what that time should be worth when I invoice. I feel like this table is more useful for reporting billable hours and invoicing.

```elisp
(defcustom td/billable-rate 80
  "The billable rate for calculating 'td/custom-clocktable"
  :type `integer
  :group 'org)

(defun td/custom-clocktable-indent (level)
  "Create an indent based on org LEVEL"
  (if (= level 1) ""
    (concat (make-string (1- level) ?—) " ")
    ))

(defun td/custom-clocktable-get-prop (key props)
  "Get a specific value using a KEY from a list of PROPS"
  (cdr (assoc key props)))

(defun td/minutes-to-billable (minutes &optional rate)
  "Get the amount in dollers that a number of MINUTES is worth"
  (let* ((hours (/ (round (* (/ minutes 60.0) 100)) 100.0))
         (amount (* hours (cond ((numberp rate) rate)
                                ((numberp td/billable-rate) td/billable-rate)
                                (0))))
         (billable (/ (round (* amount 100)) 100.0)))
    billable))

(defun td/emph-str (string &optional emph)
  "Emphasize a STRING if EMPH is set"
  (if emph
      (format "*%s*" string)
    string))

(defun td/custom-clocktable (ipos tables params)
  "An attempt to clock my voltage time, my way"
  (let* ((lang (or (plist-get params :lang) "en"))
         (block (plist-get params :block))
         (emph (plist-get params :emphasize))
         (header (plist-get params :header))
         (properties (or (plist-get params :properties) '()))
         (comments-on (member "Comment" properties))
         (formula (plist-get params :formula))
         (rate (plist-get params :rate))
         (has-formula (cond ((and formula (stringp formula))
                             t)
                            (formula (user-error "Invalid :formula param"))))
         (effort-on (member "Effort" properties)))
    (goto-char ipos)

    (insert-before-markers
     (or header
         ;; Format the standard header.
         (format "#+CAPTION: %s %s%s\n"
                 (org-clock--translate "Clock summary at" lang)
                 (format-time-string (org-time-stamp-format t t))
                 (if block
                     (let ((range-text
                            (nth 2 (org-clock-special-range
                                    block nil t
                                    (plist-get params :wstart)
                                    (plist-get params :mstart)))))
                       (format ", for %s." range-text))
                   "")))
     "| Task " (if effort-on "| Est" "")
     "| Time | Billable"
     (if comments-on "| Comment" "") "\n")
    (let '(total-time (apply #'+ (mapcar #'cadr tables)))
      (when (and total-time (> total-time 0))
        (pcase-dolist (`(, file-name , file-time , entries) tables)
          (when (and file-time (> file-time 0))
            (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
              (insert-before-markers
               (if (= level 1) "|-\n|" "|")
               (td/custom-clocktable-indent level)
               (concat (td/emph-str headline (and emph (= level 1))) "|")
               (if-let* (effort-on
                         (eft (td/custom-clocktable-get-prop "Effort" props))
                         (formatted-eft (org-duration-from-minutes
                                         (org-duration-to-minutes eft))))
                   (concat (td/emph-str formatted-eft (and emph (= level 1)))
                           "|")
                 (if effort-on "|"
                   ""))
               (concat (td/emph-str
                        (org-duration-from-minutes time)
                        (and emph (= level 1))) "|")
               (concat (td/emph-str
                        (format "$%.2f" (td/minutes-to-billable time rate))
                        (and emph (= level 1))) "|")
               (if-let* (comments-on
                         (comment
                          (td/custom-clocktable-get-prop "Comment" props)))
                   (concat comment "\n")
                 "\n")))))
        (let ((cols-adjust
               (if (member "Effort" properties)
                   2
                 1)))
          (insert-before-markers
           (concat "|-\n| "
                   (td/emph-str "Totals" emph)
                   (make-string cols-adjust ?|))
           (concat (td/emph-str
                    (format "%s" (org-duration-from-minutes total-time)) emph)
                   "|")
           (concat (td/emph-str
                    (format "$%.2f" (td/minutes-to-billable total-time rate))
                    emph) "|" ))
          (when has-formula
            (insert "\n#+TBLFM: " formula)))))
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when has-formula (org-table-recalculate 'all))))

(defun td/clocktable-format-toggle ()
  (interactive)
  (if (equal org-duration-format '((special . h:mm)))
      (setq-local org-duration-format '(("h" . nil) (special . 2)))
    (setq-local org-duration-format '((special . h:mm))))
  (org-ctrl-c-ctrl-c))
```

Here's an example:

| Task              | Est   | Time   | Billable | Comment                |
|----------------- |----- |------ |-------- |---------------------- |
| Client            |       | 8.00h  | $520.00  |                        |
| — Task B          |       | 2.00h  | $130.00  | This is taking a while |
| — Task A          |       | 6.00h  | $390.00  |                        |
| Client B          |       | 12.43h | $807.95  |                        |
| — Special Project |       | 12.00h | $780.00  |                        |
| —— Task C         | 9.00h | 8.00h  | $520.00  |                        |
| —— Task D         |       | 4.00h  | $260.00  |                        |
| — Unrelated Task  |       | 0.43h  | $27.95   |                        |
| Totals            |       | 20.43h | $1327.95 |                        |


<a id="orgc4f2f2e"></a>

#### Ledger

Knowing what resources you have at your disposal and learning how to budget are powerful things.

```elisp
ledger-mode
```

```elisp
(setq ledger-use-native-highlighting t)
```


<a id="org60a40c3"></a>

#### Vterm     :guix:

A "normal" terminal for Emacs. This package is currently installed by the guix system.

```elisp
(td/bind-keys '(("C-c v t" . multi-vterm)
                ("C-c v n" . multi-vterm-next)
                ("C-c v p" . multi-vterm-prev)
                ("C-c v d" . multi-vterm-dedicated-toggle)
                ("C-c v P" . multi-vterm-project)))
```


<a id="org406badd"></a>

#### Notmuch

Notmuch is a really impressive way to read and organize mail via tagging files. It works really quickly and the configuration is really flexible.


<a id="org8d77534"></a>

##### Built In Mail Settings

```elisp
(setq send-mail-function 'sendmail-send-it
      sendmail-program "~/.guix-home/profile/bin/msmtp"
      message-directory "~/.local/share/mail"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      message-signature-directory "~/.local/share/mail/signatures"
      message-signature-file "default")
```


<a id="org87e7497"></a>

##### Notmuch

```elisp
notmuch
```

```elisp
(require 'notmuch)

(setq notmuch-fcc-dirs
      '(("trev@fastmail.com" . "fastmail/Sent")
        ("trev@trevdev.ca"   . "fastmail/Sent")
        ("tn@eml.cc"         . "fastmail/Sent")
        ("trevor@voltagenewmedia.com" . "voltage/Sent"))
      notmuch-saved-searches '(
                               (:name "todo"
                                      :query "tag:todo"
                                      :key "t"
                                      :sort-order newest-first)
                               (:name "flagged"
                                      :query "tag:flagged"
                                      :key "f"
                                      :sort-order newest-first)
                               (:name "personal"
                                      :query "not tag:work"
                                      :count-query "not tag:work and tag:unread"
                                      :key "p"
                                      :sort-order newest-first)
                               (:name "work"
                                      :query "tag:work"
                                      :count-query "tag:work and tag:unread"
                                      :key "w"
                                      :sort-order newest-first)
                               (:name "drafts"
                                      :query "tag:draft"
                                      :key "d"
                                      :sort-order newest-first)
                               (:name "sent"
                                      :query "tag:sent"
                                      :count-query "tag:nil"
                                      :key "s"
                                      :sort-order newest-first)
                               (:name "archive"
                                      :count-query "tag:nil"
                                      :query "tag:archive"
                                      :key "a"
                                      :sort-order newest-first)
                               (:name "all mail"
                                      :query "*"
                                      :count-query "tag:nil"
                                      :key "A"
                                      :sort-order newest-first))
      notmuch-archive-tags '("+archive" "-inbox")
      notmuch-tagging-keys '(("a" notmuch-archive-tags "Archive")
                             ("u" notmuch-show-mark-read-tags "Mark read")
                             ("f" ("+flagged") "Flag")
                             ("s" ("+spam" "-inbox") "Mark as spam")
                             ("d" ("+deleted" "-inbox") "Delete"))
      notmuch-show-logo nil
      notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full
      notmuch-hello-thousands-separator ",")

(global-set-key (kbd "<f5>") #'notmuch)

(defun td/specify-msmtp-account ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward "From:")
    (setq message-sendmail-extra-arguments
          (if (string-match-p (regexp-quote "voltagenewmedia")
                              (thing-at-point 'line t))
              (list "-a" "voltage")
            (list "-a" "default")))))

(add-hook 'notmuch-mua-send-hook #'td/specify-msmtp-account)
```


<a id="org2ff58b1"></a>

##### org-mime

Edit messages using org-mode.

```elisp
org-mime
```

```elisp
(autoload 'org-mime-edit-mail-in-org-mode "org-mime"
  "Edit a message in org-mode"
  t)

(setq org-mime-export-options
      '(:with-latex dvipng :section-numbers nil :with-author nil :with-toc nil))

(td/bind-keys '(("C-c C-o" . org-mime-edit-mail-in-org-mode)
                ("C-c C-h" . org-mime-htmlize))
              message-mode-map)
```


<a id="orgc17e144"></a>

##### org-contacts

Organize contacts with org-mode.

```elisp
org-contacts
```

```elisp
(quelpa-get org-contacts
            :fetcher git
            :url "https://repo.or.cz/org-contacts.git")

(require 'org-contacts)
(setq org-contacts-files '("~/Org/contacts.org"))
```


<a id="org1219773"></a>

#### Password Store

```elisp
password-store
```

```elisp
(td/bind-keys '(("C-c p c" . password-store-copy)
                ("C-c p f" . password-store-copy-field)
                ("C-c p i" . password-store-insert)
                ("C-c p g" . password-store-generate)))
```


<a id="org70cffe1"></a>

#### Sensitive Mode

Inspired from a script written by [Anirudh Sasikumar](https://anirudhsasikumar.net/blog/2005.01.21.html). It has been adapted to accomodate undo-tree. This prevents emacs from generating unencrypted backups & autosave data from `.gpg` files.

```elisp
(define-minor-mode sensitive-mode
  "A minor-mode for preventing auto-saves and back-ups for encrypted files."
  :global nil
  :lighter " Sensitive"
  :init-value nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1))
        ;; disable undo-tree history(?)
        (when (bound-and-true-p undo-tree-mode)
          (undo-tree-mode -1)))
    (kill-local-variable 'backup-inhibited)
    (if auto-save-default
        (auto-save-mode 1))
    (when (bound-and-true-p global-undo-tree-mode)
      (undo-tree-mode 1))))
```


<a id="org3d118a8"></a>

#### RG

```elisp
rg
```

```elisp
(rg-enable-default-bindings)
```


<a id="org330c0c4"></a>

#### Visual Fill Column

Creates a fake "fill column" to wrap text around. Makes reading documents more visually appealing without breaking text into newlines.

```elisp
visual-fill-column
```

```elisp
(defun td/visual-fill-setup ()
  "Center the column 100 characters wide."
  (setq-local visual-fill-column-width 100
              visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c v") #'visual-fill-column-mode))

(add-hook 'org-mode-hook #'td/visual-fill-setup)
```


<a id="org7287c57"></a>

#### Which-key

What the heck was that keybind again? If you can remember how it starts, which-key can help you find the rest.

```elisp
which-key
```

```elisp
(which-key-mode)
```


<a id="org4d282eb"></a>

#### Windmove

Set up a keymap for wind-move and bind it to a prefix that's easy to hit.

```elisp
(defvar td/windmove-map
  (let ((map (make-sparse-keymap)))
    (td/bind-keys '(("e"   . windmove-right)
                    ("a"   . windmove-left)
                    ("n"   . windmove-down)
                    ("p"   . windmove-up)
                    ("s e" . windmove-swap-states-right)
                    ("s a" . windmove-swap-states-left)
                    ("s n" . windmove-swap-states-down)
                    ("s p" . windmove-swap-states-up)
                    ("d e" . windmove-delete-right)
                    ("d a" . windmove-delete-left)
                    ("d n" . windmove-delete-down)
                    ("d p" . windmove-delete-up)
                    ("d d" . delete-window)
                    ("D" . delete-other-windows)
                    ("o"   . other-window)
                    ("v"   . split-window-right)
                    ("h"   . split-window-below)
                    ("="   . enlarge-window)
                    ("-"   . shrink-window)
                    ("b"   . balance-windows))
                  map)
    map)
  "A keymap for windmove functions.
\\{td/windmove-map}")

(fset 'td/windmove-map td/windmove-map)

(global-set-key (kbd "M-o") td/windmove-map)
```


<a id="org113a830"></a>

### Syntax Support

This section is for syntax highlighting and language specific tooling.


<a id="org86100c5"></a>

#### Clojure

This configuration includes clojure-mode and cider.

```elisp
clojure-mode
cider
```

```elisp
(td/auto-mode '(("\\.clj\\'" . clojure-mode)))
```


<a id="orge51cb28"></a>

#### Common Lisp

The most important package to have handy for Common Lisp is the "slime" package.

```elisp
sly
```


<a id="org22a9399"></a>

#### CSS/SCSS

```elisp
(setq css-indent-offset 2
      tab-width 2)
```


<a id="org3e77fed"></a>

#### Eglot

Eglot - the rival LSP client to the infamous `lsp-mode`. Eglot claims to be leaner, faster and less intense.

```elisp
eglot
```

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(shopify-mode
                 . ("theme-check-language-server" "--stdio")))

  (defvar td/eglot-funcs
    (let ((map (make-sparse-keymap)))
      (td/bind-keys '(("C-r"   . eglot-rename)
                      ("C-d"   . eglot-find-typeDefinition)
                      ("C-S-d" . eglot-find-declaration)
                      ("C-f"   . eglot-format)
                      ("C-S-f" . eglot-format-buffer)
                      ("C-S-r" . eglot-reconnect)) map)
      map) "Custom keybinds for eglot functions. \\{td/eglot-funcs}")
  (fset 'td/eglot-funcs td/eglot-funcs)
  (define-key eglot-mode-map (kbd "C-c C-e") 'td/eglot-funcs)

  (setq eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.7
        eglot-autoshutdown t)

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              "Make sure Eldoc will show us all of the feedback at point."
              (setq-local eldoc-documentation-strategy
                          #'eldoc-documentation-compose))))
```


<a id="orgf1e2c3b"></a>

#### Eldoc

```elisp
(setq eldoc-echo-area-use-multiline-p nil
      eldoc-documentation-strategy 'eldoc-documentation-compose)
```


<a id="org566ffff"></a>

#### Eldoc Box

```elisp
eldoc-box
```

```elisp
(autoload 'eldoc-box-help-at-point "eldoc-box.el"
  "Activate pop-up for eldoc information for the thing at point."
  t)

(global-set-key (kbd "C-c M-h") #'eldoc-box-help-at-point)
```


<a id="org2f6640b"></a>

#### Emmet

`.Emmet[data-love="true"]`

```elisp
emmet-mode
```

```elisp
(setq emmet-expand-jsx-className t)
(td/add-hooks '(sgml-mode
                css-mode
                web-mode
                svelte-mode)
              #'emmet-mode)
```


<a id="orge3f707c"></a>

#### GoLang

```elisp
go-mode
```

```elisp
(td/auto-mode '(("\\.go\\'" . go-mode)))
```


<a id="orgb3f7081"></a>

#### Lua Mode

```elisp
lua-mode
```

```elisp
(td/auto-mode '(("\\.lua\\'" . #'lua-mode)))
```


<a id="orgf76b6ce"></a>

#### Markdown

The free software documentation language of the Internet.

```elisp
markdown-mode
```

```elisp
(td/auto-mode '(("README\\.md\\'" . gfm-mode)
                ("\\.md\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)))
```


<a id="orged0968e"></a>

#### Nim

```elisp
nim-mode
```


<a id="org631651a"></a>

#### Paredit

```elisp
paredit
```

```elisp
(td/add-hooks '(lisp-mode
                scheme-mode
                clojure-mode
                emacs-lisp-mode)
              #'enable-paredit-mode)
```


<a id="orgf23f861"></a>

#### PHP

```elisp
php-mode
```

```elisp
(defun td/get-intelephense-key ()
  "Get my intelephense license key."
  (with-temp-buffer
    (insert-file-contents "~/Documents/intelephense.txt")
    (buffer-string)))

(defun td/get-wordpress-stubs ()
  "The stubs required for a WordPress Project"
  (json-insert ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core"
                "ctype" "curl" "date" "dba" "dom" "enchant" "exif"
                "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap"
                "interbase" "intl" "json" "ldap" "libxml" "mbstring"
                "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl"
                "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql"
                "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline"
                "recode" "Reflection" "regex" "session" "shmop" "SimpleXML"
                "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard"
                "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy"
                "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter"
                "Zend OPcache" "zip" "zlib" "wordpress"]))
```


<a id="org3587ae1"></a>

#### Prettier

An opinionated way to clean up my web-dev code quickly.

```elisp
prettier-js
```


<a id="orgf871173"></a>

#### Python

<3 Python

```elisp
pyvenv
```


<a id="org3026418"></a>

#### Rainbow Delimiters

This comes in handier than you think it would. Especially with these

```elisp
rainbow-delimiters
```

```elisp
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
```


<a id="orgd5d861d"></a>

#### Rainbow Mode

LSP-Mode covers making visual representations of hex color codes almost everywhere I need it. For everywhere else there's rainbow-mode

```elisp
rainbow-mode
```


<a id="orgb00386c"></a>

#### Ruby

```elisp
inf-ruby
```


<a id="org6f89698"></a>

#### Rust

```elisp
rust-mode
```

```elisp
(defun td/rust-run-args (s)
  (interactive "sOptional Args:")
  (rust--compile (concat "%s run " s) rust-cargo-bin))

(with-eval-after-load 'rust-mode
  (td/bind-keys '(("C-c c r" . rust-run)
                  ("C-c c a r" . td/rust-run-args))
                rust-mode-map))
```


<a id="org53f5973"></a>

#### Scheme

There are many dialects of Scheme. I am choosing to organize mine in this subcategory.

Guile: GNU Ubiquitous Intelligent Language for Extensions

```elisp
geiser-guile
```


<a id="org43eb35e"></a>

#### Shopify Mode

This is where I turn emacs into a usuable IDE for Shopify themes. I use regexp to tell emacs to use s/css-mode for css liquid, then register an LSP client for the [theme-check-language-server](https://shopify.dev/themes/tools/theme-check#using-theme-check-in-other-editors).

```elisp
(define-derived-mode shopify-mode web-mode "Shopify"
  "Use web mode to highlight shopify liquid files")
(provide 'shopify-mode)
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . shopify-mode))
(defvar liquid-electric-pairs '((?% . ?%))
  "Electric pairs for liquid syntax.")
(defun liquid-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs
                                          liquid-electric-pairs)
              electric-pair-text-pairs electric-pair-pairs))
(add-hook 'shopify-mode-hook #'liquid-add-electric-pairs)
(add-to-list 'org-src-lang-modes '("liquid" . shopify))
```


<a id="orgcb67f27"></a>

#### Svelte

Fake-out a "svelte-mode" for the purposes of activating with the svelte-language-server. I'm extending web-mode because it highlights `.svelte` files well.

```elisp
(define-derived-mode svelte-mode web-mode "Svelte"
  "I just want web-mode highlighting with .svelte files")
(provide 'svelte-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
```


<a id="org848b8bc"></a>

#### Treesitter

Tree-sitter is an impressive project. It delivers exceptionally rich syntax highlighting for things like emacs/vim. A little tricky to theme, though, as it has a billion font lock faces and every tree-sitter syntax config may or may not use them the same way. I try to avoid looking a gift horse in the mouth.

```elisp
tree-sitter
tree-sitter-langs
```

```elisp
(defun td/start-tree-sitter ()
  "Fires up tree-sitter for select modes"
  (tree-sitter-mode)
  (tree-sitter-hl-mode))

(td/add-hooks '(js-mode
                typescript-mode
                css-mode
                rust-mode)
              #'td/start-tree-sitter)
```


<a id="org4c1c99f"></a>

#### TypeScript & JavaScript

```elisp
typescript-mode
```

```elisp
(setq js-indent-level 2)
(setq typescript-indent-level 2)
```


<a id="org7004dc7"></a>

#### VueJS

```elisp
(define-derived-mode vue-mode web-mode "VueJS"
  "I just want web-mode highlighting with .svelte files")
(provide 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
```


<a id="org13aa50a"></a>

#### Web Mode

There isn't a much better catch-all for web template syntax support than web-mode. It works well with Liquid syntax files. It also comes with it's own divergent, insane defaults that I have to choke out.

```elisp
web-mode
```

```elisp
(td/auto-mode '(("\\.html\\'" . web-mode)))
(setq web-mode-markup-indent-offset tab-width
      web-mode-code-markup-indent-offset tab-width
      web-mode-style-padding tab-width
      web-mode-script-padding tab-width
      web-mode-block-padding tab-width
      web-mode-enable-auto-indentation nil
      web-mode-enable-auto-pairing nil)
(add-to-list 'org-src-lang-modes '("html" . web))
```


<a id="org9fc16ea"></a>

#### YAML

YAML's a really nice way to configure software, containers and projects. I use it when I can.

```elisp
yaml-mode
```

```elisp
(td/auto-mode '(("\\.yml\\'" . yaml-mode)))
```


<a id="org991b107"></a>

### Load Customizer Settings

Load the file we created for custom vars in the [general settings](#org8ec129b).

```elisp
(load custom-file 'noerror 'nomessage)
```


<a id="org6508ba8"></a>

## About This Config

This literate configuration is a labour of love from a man who changes his mind and mixes things up *often*.

I'm not sure it will ever be finished or perfect. At times, things may clunk. I will do my best to clunk them in another branch.

If you like this config the way you found it, make sure that you fork it or make note of which commit you preferred.

If you like it enough to drop me a tip, feel free to do so:

[![img](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y34UWHH) [![img](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/trev.dev/donate) BTC: bc1qwad2jlteldw644w4wfh28y6ju53zfp69nnswrq


<a id="orgd63c4c1"></a>

### Installation

If you've decided to fork this repository and wish to use it as-is, here are the steps you'll need to take.

1.  Clone this repository somewhere.
2.  Tangle config.org. The resulting configuration files should be output to `.emacs.d/*.el`
3.  Symlink, copy or move the config files to wherever you want to start your init.


<a id="orgdf2956e"></a>

### Licenses

-   For the [bell sound](inspectorj_bell.wav): "Bell, Candle Damper, A (H4n).wav" by InspectorJ (www.jshaw.co.uk) of Freesound.org (Creative Commons - CC BY 3.0
