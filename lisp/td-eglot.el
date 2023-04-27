;;; td-eglot.el --- LSP configuraton -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file is for setting up Emacs' built in LSP.
;;
;;; Code:
(require 'td-helpers)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(shopify-mode
                 . ("shopify" "store" "language-server")))

  (defvar td/eglot-funcs
    (let ((map (make-sparse-keymap)))
      (td-bind-keys '(("r"   . eglot-rename)
                      ("d"   . eglot-find-typeDefinition)
                      ("S-d" . eglot-find-declaration)
                      ("f"   . eglot-format)
                      ("S-f" . eglot-format-buffer)
                      ("S-r" . eglot-reconnect))
                    map)
      map)
    "Custom keybinds for eglot functions. \\{td/eglot-funcs}")
  (fset 'td/eglot-funcs td/eglot-funcs)
  (define-key eglot-mode-map (kbd "C-c e") 'td/eglot-funcs)

  (setq eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.7
        eglot-autoshutdown t))

(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'eglot-java-mode-hook
          #'(lambda ()
              (td-bind-keys '(("C-c e n" . eglot-java-file-new)
                              ("C-c e x" . eglot-java-run-main)
                              ("C-c e t" . eglot-java-run-test)
                              ("C-c e N" . eglot-java-project-new)
                              ("C-c e T" . eglot-java-project-build-task)
                              ("C-c e R" . eglot-java-project-build-refresh)))
              eglot-java-mode-map))

(provide 'td-eglot)
;;; td-eglot.el ends here
