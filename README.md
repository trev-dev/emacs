# Emacs Configuration


The following document is a literate program that will generate my `~/.emacs.d`. It is intended to be used in conjunction with a dotfile manager such as GNU Stow for symlinking back to the home directory. You could, however, tangle this file from the home directory and skip using stow.

To read more about how this configuration is supposed to work, see my [config strategy](#org529057f).

**Table of Contents**

1.  [Configuration Strategy](#org529057f)
    1.  [Tangling Files](#orgcc24448)
    2.  [Configuring Packages](#org368f6d8)
2.  [Early Init](#orgc430030)
3.  [Init File Headers](#org7f60d6c)
4.  [General Settings](#orgdec9293)
5.  [Helper Functions](#orge6daa8e)
    1.  [Priority Mode](#org09e2867)
6.  [Keybinds](#orgd9069b0)
7.  [Prog Mode](#org621c1ae)
8.  [Package Configuration](#org4f432b6)
    1.  [Look & Feel](#orgcd938c0)
        1.  [All The Icons](#org1c67923)
        2.  [Dired](#org8fee9d9)
        3.  [Diminish](#org3b2eff8)
        4.  [Custom Theme Devel](#orgf0044fa)
        5.  [Font Setup](#org02be9f4)
        6.  [Modeline](#orgb6de278)
    2.  [Utility Packages](#org7297c86)
        1.  [Avy](#org6ddc0f2)
        2.  [CTRLF](#org14a3a04)
        3.  [Completions](#org8735bba)
            1.  [Company](#org36ee17e)
            2.  [Icomplete mode](#orgc474209)
            3.  [Savehist](#org4388747)
        4.  [Docker](#org4c659e5)
        5.  [Diff-hl](#org04d166a)
        6.  [Elfeed](#org0be6a60)
        7.  [EMMS](#org4aed1bf)
        8.  [Surround](#orgb53cced)
        9.  [ERC](#org832da8e)
        10. [Eshell](#org613be33)
        11. [Expand Region](#orgc3498a7)
        12. [God Mode](#org6edf562)
            1.  [Functions](#org0827a17)
            2.  [Insert Ahead](#org590142d)
            3.  [Org Mode Newline Advice](#org1118158)
            4.  [Seeking Characters](#orgc6064a1)
            5.  [Cursor Indicator](#orgf107b21)
            6.  [Keybindings](#orgbec2a1d)
            7.  [Apply & Finish Setup](#orgec52111)
        13. [Goggles](#org8956f51)
        14. [Imenu](#org4b9a595)
        15. [Magit](#org5070c22)
        16. [Mastodon](#org1951c3a)
        17. [Multiple Cursors](#org5f03417)
        18. [Org](#orge6350da)
            1.  [Key Variables](#org353dbde)
            2.  [Functions](#org509aa1d)
            3.  [Apply Configuration](#org1231734)
            4.  [Extending Org Mode](#org99e3124)
        19. [Ledger](#org1f8f824)
        20. [Vterm](#orgcf41df4):guix:
        21. [Notmuch](#orgbbcd2eb)
            1.  [Built In Mail Settings](#orgc5fa1b5)
            2.  [Notmuch](#org61c3b2d)
            3.  [org-mime](#orgd42b189)
        22. [Password Store](#org286d766)
        23. [Sensitive Mode](#org5b105f8)
        24. [RG](#org876cb1d)
        25. [Visual Fill Column](#org268eec2)
        26. [Which-key](#org23f469a)
        27. [Windmove](#org963452c)
    3.  [Syntax Support](#orgbd74395)
        1.  [Clojure](#orga612944)
        2.  [Common Lisp](#orgf9a2128)
        3.  [CSS/SCSS](#org725a360)
        4.  [Emmet](#org62d4653)
        5.  [Flycheck](#org6eb7b43)
        6.  [LSP Mode](#orgaab1376)
        7.  [Markdown](#orgaa08e98)
        8.  [Paredit](#org067f7da)
        9.  [PHP](#orga20ca4a)
        10. [Prettier](#org7564fea)
        11. [Python](#orged08b38)
        12. [Rainbow Delimiters](#org2b0b2a0)
        13. [Rainbow Mode](#org27a1f67)
        14. [Ruby](#org004a1b4)
        15. [Rust](#orgc1f6132)
        16. [Scheme](#org19e2aab)
        17. [Shopify Mode](#org4495b3e)
        18. [Svelte](#org2436497)
        19. [TypeScript & JavaScript](#orga2f95de)
        20. [VueJS](#orga223c71)
        21. [Web Mode](#orge460a83)
        22. [YAML](#org5b68524)
        23. [Yasnippet](#org334b1aa)
    4.  [Load Customizer Settings](#org64adf7e)
9.  [About This Config](#org3a19359)
    1.  [Installation](#org341e0c2)
    2.  [Licenses](#org9090884)


<a id="org529057f"></a>

## Configuration Strategy

The goal of my configuration strategy is to create a more re-producible "hardened" Emacs configuration with the help of the Guix package manager. I will be using `straight.el` for interim packages that are not yet "stable" or perhaps are not yet added to Guix.

The long-term vision is to have a config that I am mostly happy with, that does not require much updating and is essentially a Guix package itself.


<a id="orgcc24448"></a>

### Tangling Files

I will be using `org-babel-tangle` to generate numerous files and to help separate concerns. This file will generate the `early-init.el`, and the `init.el` file.


<a id="org368f6d8"></a>

### Configuring Packages

Since this init file is a the "build source" of a Guix package, it is assumed that all dependencies will already be installed before runtime is initiated, or by `straight.el` as needed.

The rest of the source code blocks in this file will be simple package configurations.


<a id="orgc430030"></a>

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
(setq package-enable-at-startup nil)
```

```elisp
;;; early-init.el ends here
```


<a id="org7f60d6c"></a>

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


<a id="orgdec9293"></a>

## General Settings

Some general performance based improvements concerning large files, when to compile with the fancy new native-comp feature and reset the GC collection size after init.

```elisp
(defvar emacs-startup-time (current-time)
  "When Emacs last initialized.")

(defun td/calculate-init-time (start-time)
  (float-time (time-subtract (current-time) start-time)))

(defun td/report-init-time ()
  "Prints the init time into the scratch buffer as a comment."
  (switch-to-buffer "*scratch*")
  (insert-before-markers
   (format ";; Init finished in %0.2f seconds; welcome to Emacs.\n"
           (td/calculate-init-time emacs-startup-time))))

(add-hook 'after-init-hook #'td/report-init-time)

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
      ispell-dictionary "en"
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


<a id="orge6daa8e"></a>

## Helper Functions

Many people configure their emacs with the help of macros such as `use-package`. I prefer to keep my init lower-level and simple. I have written some simple functions that will make configuration less repetitive.

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


<a id="org09e2867"></a>

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


<a id="orgd9069b0"></a>

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


<a id="org621c1ae"></a>

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


<a id="org4f432b6"></a>

## Package Configuration

This section is for configuring packages, both built in and abroad.


<a id="orgcd938c0"></a>

### Look & Feel


<a id="org1c67923"></a>

#### All The Icons

```elisp
(require 'all-the-icons)
```


<a id="org8fee9d9"></a>

#### Dired

```elisp
all-the-icons-dired
```

```elisp
(setq dired-dwim-target t)
(with-eval-after-load 'all-the-icons
  (setq all-the-icons-dired-monochrome nil)
  (add-hook
   'dired-mode-hook #'(lambda ()
                        (when (display-graphic-p)
                          (all-the-icons-dired-mode))
                        (dired-hide-details-mode))))
```


<a id="org3b2eff8"></a>

#### Diminish

Output from the `minor-mode-alist`. Due to how lazy-loading works, we want to make sure we have diminish early on.

```elisp
(defun tdm/diminish-lsp-lighter ()
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
                ("flymake" 'flymake-mode)
                ("flycheck" 'flycheck-mode)
                ("evil-org" 'evil-org-mode)
                ("lsp-mode" 'lsp-mode '(:eval (tdm/diminish-lsp-lighter)))
                ("tree-sitter" 'tree-sitter-mode "TS")
                ("god-mode" 'god-local-mode)
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


<a id="orgf0044fa"></a>

#### Custom Theme Devel

I've written my own theme called "tangonov".

```elisp
(add-to-list 'custom-theme-load-path "~/Projects/tangonov-theme/")
(setq tangonov-selection-foregrounds nil)
(defun td/load-theme (frame)
  "Load the theme correctly for a `FRAME' if we're using emacsclient."
  (select-frame frame)
  (load-theme 'tangonov t))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'td/load-theme)
  (load-theme 'tangonov t))
```


<a id="org02be9f4"></a>

#### Font Setup

```elisp
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(add-to-list 'default-frame-alist '(font . "Hack 12"))
```


<a id="orgb6de278"></a>

#### Modeline

```elisp
(defvar-local tdm/git-cached-status nil
  "The buffer's last known workspace status.")

(defun tdm/git-cache-status (&rest args)
  "Set local buffer's git cache status.
Accepts 'ARGS' but does not use them."
  (when-let ((buffer (and (project-current)
                          (member (buffer-file-name)
                                  (project-files (project-current)))
                          (buffer-file-name))))
    (setq tdm/git-cached-status
          (vc-state-refresh buffer 'git))))

(add-hook 'after-save-hook #'tdm/git-cache-status)

(add-to-list 'window-buffer-change-functions #'tdm/git-cache-status)

(defvar tdm/git-status-plist '(unregistered ("  ⁈" . (:foreground "#C792EA"))
                               edited ("  ±" . (:foreground "#82AAFF"))
                               up-to-date ("  ✔" . success)
                               ignored ("  ।" . warning)))

(defun tdm/vc ()
  "Get the git status for the current buffer."
  (when-let ((styles (plist-get tdm/git-status-plist tdm/git-cached-status)))
    (let* ((icon (car styles))
           (color (cdr styles))
           (branch (if vc-mode (substring vc-mode 5) "untracked")))
      `(:propertize ,(concat icon " " branch) face ,color))))

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

(require 'svg-lib)
(add-to-list 'svg-lib-icon-collections
             (cons "local" "file:///home/trevdev/.config/emacs/%s.svg"))

(defun tdm/god-mode-icon ()
  "Retrieve the hammer of the gods."
  (svg-lib-icon "mjolnir"
                `(:collection "local"
                              :stroke 0
                              :foreground ,(face-foreground 'warning)
                              :background ,(face-background 'mode-line))))

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
                      (propertize "God" 'display (tdm/god-mode-icon))
                    ""))))

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
                     (:eval (tdm/meow-state))
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
                   '((:eval (tdm/god-state))
                     (:eval (tdm/macro-indicator))
                     (:eval (tdm/flycheck))
                     (:eval (tdm/vc))
                     (:eval (tdm/misc))
                     "  "
                     mode-line-modes))))))
```


<a id="org7297c86"></a>

### Utility Packages

Packages that extend and augment emacs in a general way


<a id="org6ddc0f2"></a>

#### Avy

```elisp
(define-key priority-mode-map (kbd "C-:") #'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "C-:") #'avy-isearch)
(avy-setup-default)
```


<a id="org14a3a04"></a>

#### CTRLF

CTRLF greatly enhances isearch.

```elisp
(ctrlf-mode 1)
```


<a id="org8735bba"></a>

#### Completions

A combination of packages to enhance completions.


<a id="org36ee17e"></a>

##### Company

Completions at point/region.

```elisp
(defun td/company-prog-hook ()
  "Completions for programming."
  (setq-local company-backends
              '(company-capf :with
                             company-yasnippet
                             company-dabbrev-code
                             company-files))
  (company-mode))

(defun td/company-text-hook ()
  "Completions for writing."
  (company-mode))

(add-hook 'prog-mode-hook #'td/company-prog-hook)
(add-hook 'text-mode-hook #'td/company-text-hook)

(setq company-files-exclusions '(".git/")
      company-idle-delay 0.3)
```


<a id="orgc474209"></a>

##### Icomplete mode

```elisp
(icomplete-mode 1)
(setq icomplete-show-matches-on-no-input t)
(td/bind-keys '(("C-n"        . icomplete-forward-completions)
                ("C-p"        . icomplete-backward-completions)
                ("S-<return>" . icomplete-force-complete-and-exit))
              icomplete-minibuffer-map)
```


<a id="org4388747"></a>

##### Savehist

Save history for Vertico to look at later.

```elisp
(savehist-mode)
```


<a id="org4c659e5"></a>

#### Docker

Numerous packages related to docker. Currently installed via Guix are:

-   emacs-docker: Control docker from Emacs
-   emacs-dockerfile-mode: Dockerfile support
-   emacs-docker-compose-mode: docker-compose.yml support


<a id="org04d166a"></a>

#### Diff-hl

Show me the diffs in the fringe!

```elisp
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
```


<a id="org0be6a60"></a>

#### Elfeed

RSS Reader :D

```elisp
(global-set-key (kbd "<f6>") #'elfeed)
```


<a id="org4aed1bf"></a>

#### EMMS

Emacs Multi-Media System

```elisp
(defun td/start-emms ()
  "Start up emms."
  (interactive)
  (require 'emms-setup)
  (require 'emms-player-mpd)

  (emms-all)

  (setq emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory "~/Music"
        emms-player-mpd-server-name "localhost")

  (add-to-list 'emms-player-list 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)

  (emms-mode-line-mode -1)
  (emms-player-mpd-connect))
```


<a id="orgb53cced"></a>

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


<a id="org832da8e"></a>

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


<a id="org613be33"></a>

#### Eshell

```elisp
;; (straight-use-package 'capf-autosuggest)

(defun td/eshell-extras ()
  "Start extra features for eshell-mode"
  (eshell-syntax-highlighting-mode)
  (capf-autosuggest-mode))

(add-hook 'eshell-mode-hook #'td/eshell-extras)
```


<a id="orgc3498a7"></a>

#### Expand Region

It just makes selecting text between sexps easy.

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


<a id="org6edf562"></a>

#### God Mode

God mode is an amazing package. It automatically translates key-chords into single-key bindings and toggled modifiers.

Because it has its own keymap, I can add utility functions to god-mode. This turns it into sort of a pseudo-modal editing mode. However, unlike other modal packages, it does not require as much key re-binding, thanks to key-chord translation.


<a id="org0827a17"></a>

##### Functions

These functions enhance editing while allowing me to "drop out" of god-mode in useful ways.

```elisp
(defun god/exit-god-local (&rest args)
  (god-local-mode -1))

(defun god/eol-insert ()
  "Move the cursor to the end-of-line and exit god mode."
  (interactive)
  (end-of-line)
  (god/exit-god-local))

(defun god/boi-insert ()
  "Move the cursor `back-to-indentation' and exit god mode."
  (interactive)
  (back-to-indentation)
  (god/exit-god-local))

(defun god/forward-insert ()
  "Move the cursor over one char and exit god mode."
  (interactive)
  (forward-char)
  (god/exit-god-local))

(defun god/change ()
  "Kill char/region and exit god mode."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (zap-to-char 1 (char-after)))
  (god/exit-god-local))

(defun god/backward-symbol (num)
  "Move backward `NUM' symbols."
  (interactive "^p")
  (forward-symbol (- 0 (or (when (natnump num) num) 1))))

(defun god/open-above ()
  "Open a new line above the current line, put the point there."
  (interactive)
  (beginning-of-line)
  (split-line)
  (god/exit-god-local))

(defun god/open-below ()
  "Open a new line below the current line, put the point there."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (god/exit-god-local))

(defun god/pull-line ()
  "Pull a line up from below the currnet line and join them."
  (interactive)
  (next-line)
  (join-line))
```


<a id="org590142d"></a>

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
              (function 'god/set-ahead-backward)))

(dolist (for-func '(forward-char
                    forward-word
                    forward-symbol
                    isearch-forward
                    isearch-forward-regexp
                    search-backward
                    search-forward-regexp))
  (advice-add for-func :after
              #'(lambda (&rest args) (god/set-ahead-direction))
              (function 'god/set-ahead-forward)))
```


<a id="org1118158"></a>

##### Org Mode Newline Advice

I would like to be able to perform special org-mode functions such as `org-meta-return` and `org-insert-todo-heading` and have `god-local-mode` turn off automatically.

```elisp
(advice-add 'org-meta-return :after
            #'god/exit-god-local
            (function 'god/insert-after-org-meta-return))

(advice-add 'org-insert-todo-heading :after
            #'god/exit-god-local
            (function 'god/insert-after-org-new-heading))

(advice-add 'org-insert-heading-respect-content :after
            #'god/exit-god-local
            (function 'god/insert-after-org-heading-respect-content))
```


<a id="orgc6064a1"></a>

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


<a id="orgf107b21"></a>

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


<a id="orgbec2a1d"></a>

##### Keybindings

Declare key-bindings to be applied in the next section.

```elisp
(defvar god/keybinds '((";" . god/repeat-seek)
                       ("A" . god/boi-insert)
                       ("B" . god/backward-symbol)
                       ("C" . god/change)
                       ("D" . delete-backward-char)
                       ("E" . god/eol-insert)
                       ("F" . forward-symbol)
                       ("g" . avy-goto-char-timer)
                       ("I" . god/insert-ahead)
                       ("i" . god-local-mode)
                       ("J" . god/pull-line)
                       ("O" . god/open-above)
                       ("o" . god/open-below)
                       ("W" . td/windmove-map)
                       ("T" . god/seek)
                       ("t" . god/seek-until)
                       ("P" . td/backward-chunk)
                       ("N" . td/forward-chunk)
                       ("(" . kmacro-start-macro)
                       (")" . kmacro-end-or-call-macro)
                       ("q" . quit-window)
                       ("z" . repeat)
                       ("," . er/keymap)))
```


<a id="orgec52111"></a>

##### Apply & Finish Setup

I want god mode to be available to me everywhere. To do this, `god-exempt-major-modes` needs to be unset before loading `god-mode`.

I would prefer to keep god mode on, or off, on a buffer-to-buffer basis. I use `god-local-mode` for this.

God has no intermediary mode for non-editing buffers. I feel like it's better to have to turn it on explicitly for quicker navigation or firing off commands.

```elisp
(setq god-mode-enable-function-key-translation nil)
(setq god-mode-alist '((nil . "C-")
                       ("m" . "M-")
                       ("M" . "C-M-")))

(require 'god-mode)
(require 'god-mode-isearch)

(dolist (mode '(notmuch-hello-mode
                notmuch-search-mode
                notmuch-show-mode))
  (add-to-list 'god-exempt-major-modes mode))

(global-set-key (kbd "C-c g") #'god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
(define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

(add-to-list 'emulation-mode-map-alists
             `((god-local-mode . ,god-local-mode-map)))

(td/bind-keys god/keybinds god-local-mode-map)

(god/cursor-toggle)

(with-eval-after-load 'which-key
  (which-key-enable-god-mode-support))

(add-hook 'post-command-hook #'god/cursor-toggle)

(when (commandp 'corfu-quit)
  (add-hook 'god-local-mode-hook #'corfu-quit))

(god-mode)
```


<a id="org8956f51"></a>

#### Goggles

Extra feedback for text changes.

```elisp
(td/add-hooks '(text-mode prog-mode) #'goggles-mode)
(setq-default goggles-pulse t)
```


<a id="org4b9a595"></a>

#### Imenu

```elisp
(global-set-key (kbd "C-c i") #'imenu)
```


<a id="org5070c22"></a>

#### Magit

Magit is one of the biggest reasons why I fell in love with emacs. It's the best keyboard driven "TUI" abstraction of the git command line anywere, period. Better than Fugitive by far. Sorry, Tim Pope.

```elisp
(global-set-key (kbd "C-c m") #'magit-status)
```


<a id="org1951c3a"></a>

#### Mastodon

Toot.

```elisp
(setq mastodon-instance-url "https://mastodon.technology"
      mastodon-active-user "trevdev"
      mastodon-tl--show-avatars t
      mastodon-media--avatar-height 30)
```


<a id="org5f03417"></a>

#### Multiple Cursors

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


<a id="orge6350da"></a>

#### Org

The greatest part of using Emacs is org-mode. It handles my agenda, my todo list, helps me prioritize tasks, track time and invoice clients.


<a id="org353dbde"></a>

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

Capture templates! These help me collect information into Org files. Currently I only have 2 cookbook capture methods that are meant to be used with org-chef. See [extensions](#org99e3124) for how I extend org-mode.

```elisp
(defvar td/capture-templates
  '(("t" "Todo" entry (file "~/Org/agenda/inbox.org")
     "* TODO %^{Title: }\n:PROPERTIES:\n:date: %U\n:END:\n%?"
     :empty-lines 1)
    ("c" "Contact" entry (file+headline "~/Org/contacts.org" "Other")
     "* %^{Name: }\n:PROPERTIES:\n:email: %?\n:END:"
     :empty-lines 1))
  "Base org-capture-templates.")

(global-set-key (kbd "C-c M-a") #'org-capture)
```

I usually stick to monospace sized fonts with the exception of Org files. I like the first 3 levels to be slightly larger than the rest, and progressively smaller. This helps me create a sense of urgency at the lower-level headers and it also improves readability.


<a id="org509aa1d"></a>

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


<a id="org1231734"></a>

##### Apply Configuration

```elisp
;; Ensure built-in org is used!
;; (straight-use-package '(org :type built-in))

(add-hook 'org-mode-hook #'td/org-hook)
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
      org-tag-alist td/tag-list
      org-todo-keywords td/todo-keywords
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-refile-targets '((org-agenda-files :maxlevel . 4)
                           ("contacts.org" :maxlevel . 1))
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


<a id="org99e3124"></a>

##### Extending Org Mode

Extending org-mode with some interesting packages.


###### ox-gfm

Get access to Github Flavored Markdown

```elisp
(with-eval-after-load 'ox
  (require 'ox-gfm))
```


###### ox-hugo

I like org-publish, but there are some files (like my cookbook) that I would like to keep in one document, as it is a capture file, and be able to easily publish it into a list of "posts".

```elisp
(with-eval-after-load 'ox
  (require 'ox-hugo))
```


###### org-present

A tiny package for presenting with org-mode.

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
  (concat "${title} "
          (propertize "${tags}" 'face 'org-tag)))

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


<a id="org1f8f824"></a>

#### Ledger

Knowing what resources you have at your disposal and learning how to budget are powerful things.

Note: this is probably built into the Guix ledger package. If something breaks, check into that.

```elisp
(setq ledger-use-native-highlighting t)
```


<a id="orgcf41df4"></a>

#### Vterm     :guix:

A "normal" terminal for Emacs. This package is currently installed by the guix system.

```elisp
(td/bind-keys '(("C-c v t" . multi-vterm)
                ("C-c v n" . multi-vterm-next)
                ("C-c v p" . multi-vterm-prev)
                ("C-c v d" . multi-vterm-dedicated-toggle)
                ("C-c v P" . multi-vterm-project)))
```


<a id="orgbbcd2eb"></a>

#### Notmuch

Notmuch is a really impressive way to read and organize mail via tagging files. It works really quickly and the configuration is really flexible.


<a id="orgc5fa1b5"></a>

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


<a id="org61c3b2d"></a>

##### Notmuch

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
      notmuch-hello-thousands-separator ","
      mml-secure-openpgp-encrypt-to-self t)

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


<a id="orgd42b189"></a>

##### org-mime

Edit messages using org-mode.

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


<a id="org286d766"></a>

#### Password Store

```elisp
(td/bind-keys '(("C-c p c" . password-store-copy)
                ("C-c p f" . password-store-copy-field)
                ("C-c p i" . password-store-insert)
                ("C-c p g" . password-store-generate)))
```


<a id="org5b105f8"></a>

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


<a id="org876cb1d"></a>

#### RG

```elisp
(rg-enable-default-bindings)
```


<a id="org268eec2"></a>

#### Visual Fill Column

Creates a fake "fill column" to wrap text around. Makes reading documents more visually appealing without breaking text into newlines.

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


<a id="org23f469a"></a>

#### Which-key

What the heck was that keybind again? If you can remember how it starts, which-key can help you find the rest.

```elisp
(which-key-mode)
```


<a id="org963452c"></a>

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


<a id="orgbd74395"></a>

### Syntax Support

This section is for syntax highlighting and language specific tooling.


<a id="orga612944"></a>

#### Clojure

This configuration includes clojure-mode and cider.

Guix Packages:

-   emacs-clojure-mode
-   emacs-cider

```elisp
(td/auto-mode '(("\\.clj\\'" . clojure-mode)))
```


<a id="orgf9a2128"></a>

#### Common Lisp

The most important package to have handy for Common Lisp is the "sly" package.

Guix Packages:

-   emacs-sly


<a id="org725a360"></a>

#### CSS/SCSS

```elisp
(setq css-indent-offset 2
      tab-width 2)
```


<a id="org62d4653"></a>

#### Emmet

`.Emmet[data-love="true"]`

Guix Packages:

-   emacs-emmet-mode

```elisp
(setq emmet-expand-jsx-className t)
(td/add-hooks '(sgml-mode
                css-mode
                web-mode
                svelte-mode)
              #'emmet-mode)
```


<a id="org6eb7b43"></a>

#### Flycheck

```elisp
(td/add-hooks '(emacs-lisp-mode prog-mode ledger-mode) #'flycheck-mode)
(global-set-key (kbd "C-c f") #'flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-checker-error-threshold 1000))
```


<a id="orgaab1376"></a>

#### LSP Mode

I prefer a lighter weight LSP. I had enjoyed Eglot for some time. LSP-Mode has better features, however. I get fairly minimal feedback about the things I care about with inline flycheck messages.

Guix Packages:

-   emacs-lsp-mode
-   emacs-lsp-ui

```elisp
(td/add-hooks '(css-mode
                scss-mode
                html-mode
                js-mode
                json-mode
                python-mode
                php-mode
                ruby-mode
                rust-mode
                scss-mode
                svelte-mode
                typescript-mode
                vue-mode
                yaml-mode)
              #'lsp)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(setq lsp-keymap-prefix "C-c l")
(setq lsp-log-io nil
      lsp-modeline-code-actions-segments '(count)
      lsp-signature-doc-lines 1
      lsp-enable-folding nil
      lsp-clients-typescript-server-args '("--stdio"
                                           "--tsserver-log-file"
                                           "/dev/stderr")
      lsp-keep-workspace-alive nil)

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-S-H") #'lsp-ui-doc-glance)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     "theme-check-language-server")
                    :activation-fn (lsp-activate-on "shopify")
                    :server-id 'theme-check))
  (add-to-list
   'lsp-file-watch-ignored-directories "[/\\]env' [/\\]__pycache__'")
  (add-to-list 'lsp-language-id-configuration
               '(shopify-mode . "shopify")))
```


<a id="orgaa08e98"></a>

#### Markdown

The free software documentation language of the Internet.

Guix Packages:

-   emacs-markdown-mode

```elisp
(td/auto-mode '(("README\\.md\\'" . gfm-mode)
                ("\\.md\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)))
```


<a id="org067f7da"></a>

#### Paredit

Guix Packages:

-   emacs-paredit

```elisp
(td/add-hooks '(lisp-mode
                scheme-mode
                clojure-mode
                emacs-lisp-mode)
              #'enable-paredit-mode)
```


<a id="orga20ca4a"></a>

#### PHP

Guix Packages:

-   emacs-php-mode

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


<a id="org7564fea"></a>

#### Prettier

An opinionated way to clean up my web-dev code quickly.

Guix Packages:

-   emacs-prettier


<a id="orged08b38"></a>

#### Python

<3 Python

Guix Packages:

-   emacs-pyvenv


<a id="org2b0b2a0"></a>

#### Rainbow Delimiters

This comes in handier than you think it would. Especially with these

Guix Package:

-   emacs-rainbow-delimiters


<a id="org27a1f67"></a>

#### Rainbow Mode

LSP-Mode covers making visual representations of hex color codes almost everywhere I need it. For everywhere else there's rainbow-mode

Guix Packages:

-   emacs-rainbow-mode


<a id="org004a1b4"></a>

#### Ruby

Guix Packages:

-   guix-inf-ruby


<a id="orgc1f6132"></a>

#### Rust

Guix Packages:

-   emacs-rust-mode

```elisp
(defun td/rust-run-args (s)
  (interactive "sOptional Args:")
  (rust--compile (concat "%s run " s) rust-cargo-bin))

(with-eval-after-load 'rust-mode
  (td/bind-keys '(("C-c c r" . rust-run)
                  ("C-c c a r" . td/rust-run-args))
                rust-mode-map))
```


<a id="org19e2aab"></a>

#### Scheme

There are many dialects of Scheme. I am choosing to organize mine in this subcategory.

Guile: GNU Ubiquitous Intelligent Language for Extensions

Guix Packages:

-   emacs-geiser-guile


<a id="org4495b3e"></a>

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
```


<a id="org2436497"></a>

#### Svelte

Fake-out a "svelte-mode" for the purposes of activating with the svelte-language-server. I'm extending web-mode because it highlights `.svelte` files well.

```elisp
(define-derived-mode svelte-mode web-mode "Svelte"
  "I just want web-mode highlighting with .svelte files")
(provide 'svelte-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
```


<a id="orga2f95de"></a>

#### TypeScript & JavaScript

Guix Packages:

-   emacs-typescript-mode

```elisp
(setq js-indent-level 2)
(setq typescript-indent-level 2)
```


<a id="orga223c71"></a>

#### VueJS

```elisp
(define-derived-mode vue-mode web-mode "VueJS"
  "I just want web-mode highlighting with .svelte files")
(provide 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
```


<a id="orge460a83"></a>

#### Web Mode

There isn't a much better catch-all for web template syntax support than web-mode. It works well with Liquid syntax files. It also comes with it's own divergent, insane defaults that I have to choke out.

Guix Packages:

-   emacs-web-mode

```elisp
(td/auto-mode '(("\\.html\\'" . web-mode)))
(setq web-mode-markup-indent-offset tab-width
      web-mode-code-markup-indent-offset tab-width
      web-mode-style-padding tab-width
      web-mode-script-padding tab-width
      web-mode-block-padding tab-width
      web-mode-enable-auto-indentation nil
      web-mode-enable-auto-pairing nil)
```


<a id="org5b68524"></a>

#### YAML

YAML's a really nice way to configure software, containers and projects. I use it when I can.

Guix Packages:

-   emacs-yaml-mode

```elisp
(td/auto-mode '(("\\.yml\\'" . yaml-mode)))
```


<a id="org334b1aa"></a>

#### Yasnippet

Snippets! They're helpful.

Guix Packages:

-   emacs-yasnippet
-   emacs-yasnippet-snippets

```elisp
(require 'yasnippet)
(global-set-key (kbd "C-c ,") #'yas-expand)
(setq yas-snippet-dirs '("~/.config/emacs/yasnippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)
```


<a id="org64adf7e"></a>

### Load Customizer Settings

Load the file we created for custom vars in the [general settings](#orgdec9293).

```elisp
(load custom-file 'noerror 'nomessage)
```


<a id="org3a19359"></a>

## About This Config

This literate configuration is a labour of love from a man who changes his mind and mixes things up *often*.

I'm not sure it will ever be finished or perfect. At times, things may clunk. I will do my best to clunk them in another branch.

If you like this config the way you found it, make sure that you fork it or make note of which commit you preferred.

If you like it enough to drop me a tip, feel free to do so:

[![img](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y34UWHH) [![img](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/trev.dev/donate) BTC: bc1qwad2jlteldw644w4wfh28y6ju53zfp69nnswrq


<a id="org341e0c2"></a>

### Installation

If you've decided to fork this repository and wish to use it as-is, here are the steps you'll need to take.

1.  Clone this repository somewhere.
2.  Tangle config.org. The resulting configuration files should be output to `.emacs.d/*.el`
3.  Symlink, copy or move the config files to wherever you want to start your init.


<a id="org9090884"></a>

### Licenses

-   For the [bell sound](inspectorj_bell.wav): "Bell, Candle Damper, A (H4n).wav" by InspectorJ (www.jshaw.co.uk) of Freesound.org (Creative Commons - CC BY 3.0
