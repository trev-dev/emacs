# My Emacs

This repository contains my Emacs init. My goal is to keep my Emacs relatively simple and reasonably modular. The goal was to go "lighter weight" as well but I still have something like [60 packages](./lisp/td-packages.el) installed. So much for that.

**Note:** This repository is a mirror and may be out of date.  I encourage you
to check out [sourcehut](https://git.sr.ht/~trevdev/emacs.d)

Anyway, here how things are laid out:

```shell
├── README.org <- You are here! Yay!
├── early-init.el # Stuff that happens just before init
├── init.el # The Emacs init file
├── lisp # Modules Folder
│   ├── priority-mode.el # An emulation layer just for top-priority keybinds
│   ├── sensitive-mode.el # Don't backup and and leak crypto files
│   ├── td-commands.el # Editor M-x commands
│   ├── td-editor-settings.el # Editor specific settings
│   ├── td-helpers.el # Init helper functions
│   ├── td-lsp.el # Versus Code features
│   ├── td-notmuch.el # Email. Kinda private, not committed
│   ├── td-org.el # The hard to plain text organization thing
│   ├── td-package-configs.el # Lots of diddly little configs
│   ├── td-packages.el # Package manifest and install plan
│   ├── td-prog-mode.el # Programmer mode stuff because programmer
│   └── td-syntax.el # Syntax specific settings and related package configs
├── mjolnir.svg # The hammer of the gods
└── yasnippets # Take a snippet, leave a snippet
```
