;;; td-lsp.el --- LSP configuraton -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file is for setting up programmer syntax specific settings/plugins
;;
;;; Code:
(require 'td-helpers)

(td-add-hooks '(css-mode
                scss-mode
                html-mode
                js-mode
                json-mode
                nim-mode
                python-mode
                php-mode
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

(add-hook 'hack-local-variables-hook
          #'(lambda () (when (derived-mode-p 'java-mode) (lsp))))

(provide 'td-lsp)
;;; td-lsp.el ends here
