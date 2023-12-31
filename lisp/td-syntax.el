;;; td-syntax.el --- Programming syntax configs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file is for setting up programmer syntax specific settings/plugins
;;
;;; Code:
(require 'td-helpers)

;;; CSS
(setq css-indent-offset 2
      tab-width 2)

;;; Emmet
(setq emmet-expand-jsx-className t)
(td-add-hooks '(sgml-mode
                css-mode
                web-mode
                svelte-mode)
              #'emmet-mode)

;;; Markdown
(td-auto-mode '(("README\\.md\\'" . gfm-mode)
                ("\\.md\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)))

;;; Paredit
(td-add-hooks '(lisp-mode
                scheme-mode
                clojure-mode
                emacs-lisp-mode)
              #'enable-paredit-mode)

;;; Rainbow Delimeters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Shopify Liquid
(define-derived-mode shopify-mode web-mode "Shopify"
  "Use web mode to highlight shopify liquid files.")
(provide 'shopify-mode)
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . shopify-mode))

(defvar liquid-electric-pairs '((?% . ?%))
  "Electric pairs for liquid syntax.")

(defun liquid-add-electric-pairs ()
  "Add additional electric pairs to shopify-mode."
  (setq-local electric-pair-pairs (append electric-pair-pairs
                                          liquid-electric-pairs)
              electric-pair-text-pairs electric-pair-pairs))

(add-hook 'shopify-mode-hook #'liquid-add-electric-pairs)

;;; Java
(add-hook 'java-ts-mode-hook #'(lambda ()
                                 (setq-local indent-tabs-mode nil
                                             java-ts-mode-indent-offset 2
                                             fill-column 120)))

;;; Svelte
(define-derived-mode svelte-mode web-mode "Svelte"
  "I just want web-mode highlighting with .svelte files.")
(provide 'svelte-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))

;;; TypeScript/JavaScript
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(setq js-indent-level 2
      typescript-indent-level 2)
(td-add-hooks '(typescript-ts-mode js-ts-mode) #'(lambda () (setq-local fill-column 120)))

;;; VueJS
(define-derived-mode vue-mode web-mode "VueJS"
  "Web-mode applied to Vue files.")
(provide 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;;; Web Mode
(td-auto-mode '(("\\.html\\'" . web-mode)))
(setq web-mode-markup-indent-offset tab-width
      web-mode-code-markup-indent-offset tab-width
      web-mode-style-padding tab-width
      web-mode-script-padding tab-width
      web-mode-block-padding tab-width
      web-mode-enable-auto-indentation nil
      web-mode-enable-auto-pairing nil)

;;; YAML Mode
(td-auto-mode '(("\\.yml\\'" . yaml-mode)))
(with-eval-after-load 'yasnippet
  (global-set-key (kbd "C-c ,") #'yas-expand)
  (setq yas-snippet-dirs '("~/.config/emacs/yasnippets"))
  (yas-reload-all))

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

;;; Treesit
;; Handles syntaxes for multiple languages with a fancy new parser.
(setq major-mode-remap-alist '((bash-mode . bash-ts-mode)
                               (typescript-mode . typescript-ts-mode)
                               (javascript-mode . js-ts-mode)
                               (json-mode . js-ts-mode)
                               (java-mode . java-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master"
                    "typescript/source")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (java "https://github.com/tree-sitter/tree-sitter-java")))


(provide 'td-syntax)
;;; td-syntax.el ends here
