;;; package --- Summary:

;;; Commentary:
"Use org to load config from ~/.dotfiles/emacs/configuration.org. First we'll need the org package. Use M-x install-package org if you do not already have it Ensure that all config files are in the correct dir. This .emacs file can then be symlinked to your home directory. Everything after ';;; emacs ends here' is auto-inserted by emacs via customize-variable. If you don't like those lazy configs just delete everything after the ends here line"

;;; Packages:
(require 'org)

"Load org config"
;;; Code:
(org-babel-load-file "~/.dotfiles/emacs/configuration.org")

(provide 'emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212121" "#f57373" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(custom-safe-themes
   '("dbf2c5c4113a1f5682880ef04506ac63fb87b418c3701aa2b2116eb93b625bb2" "f50f6d95dfbb981226834c2797defaa0450378d2eda822a95063d0b5ad550f70" "1ac92be7c6535076695a083ee634ba6d0343a859241e54df8dfb37ffcee067ac" "6bdacbca9bdd7c8ad999e676aefdc530b3b0fe84faccf6be6c561c197324c0ae" "27632c8377006fe8ee93d380d316703cb0ef9ee9a08c703d4f131ff6b4449421" "c0d19b2c585b28fc04932660f5044ee370d26aad1146d838714776483837f0b6" "da9ea8d5b2e34354f3bfe0a7858bd96a01842f6dbc512e3e51b0825f1f089315" "771eb98eb75356635be746cf6134219a70e40bd39284599379b3f84bd3ce557c" "486fbbeabc0998e927223191a376c9c029da62be4f537899fb5c5979047fb31d" "13613ae0f2e659cb556213ed16ddb85acc618108d5e512485ec2f8b6e724e30a" "3d3ffa5f4993a021eaa1273ff3bc21e64ba3bb21c6ce730b61d70c670430dcf0" "3fa9e45face7355f5870535b34a3a1940b0271c2334186355fc9ed9810217a03" "0bcde46e9be4c30ef1b21290e927bdcfe1dcb9965c0af475e77d0b3e5bbf9b60" "6d50db07a66771ef738f20bf440368df6654a23cd2514d79838870912fc42f88" "8cb8b853d2cdfa11f893976d340eb30be046d6d24f099a4808051c998bf09281" "84a3825bc0958ac7d0e5723df4d6bf3fff05e0e6aba70d88fb8a4134a1d0e4e9" "b4a533d05b04cc5900374e776b65f426c6fed20cd53d91d10aa6a0019243d7e8" "66fae4c9c75068d3ffa73ae082cc6d1f92705652017eb61c84bc23b798641dd0" "9e96da6baa79265b893f33b722648a1cc639edda622b65456d9db5dc65d53847" "d68c642bfb520c8cc223791437720e8ee7437346ed758dbac3dbd541fedca07b" "43d73e8c4d6458f0aa2928287e2e1a57f416ac48a9ba3ee15624417713784e54" default))
 '(exwm-floating-border-color "#303030")
 '(fci-rule-color "#585858")
 '(highlight-tail-colors ((("#31342b") . 0) (("#2b3337") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#c792ea"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#585858"))
 '(objed-cursor-color "#f57373")
 '(package-selected-packages
   '(yaml-mode which-key web-mode use-package treemacs-projectile treemacs-magit treemacs-icons-dired treemacs-evil tree-sitter-langs tide svelte-mode rjsx-mode ranger rainbow-mode rainbow-delimiters prettier-js ox-gfm org-bullets neotree lsp-ui lsp-ivy evil-surround evil-collection emmet-mode doom-themes doom-modeline diff-hl counsel company ace-jump-mode))
 '(pdf-view-midnight-colors (cons "#EEFFFF" "#212121"))
 '(rustic-ansi-faces
   ["#212121" "#f57373" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(vc-annotate-background "#212121")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c3e88d")
    (cons 40 "#d7de81")
    (cons 60 "#ebd476")
    (cons 80 "#ffcb6b")
    (cons 100 "#fcb66b")
    (cons 120 "#f9a16b")
    (cons 140 "#F78C6C")
    (cons 160 "#e78e96")
    (cons 180 "#d690c0")
    (cons 200 "#c792ea")
    (cons 220 "#d687c2")
    (cons 240 "#e57d9a")
    (cons 260 "#f57373")
    (cons 280 "#cd6c6c")
    (cons 300 "#a66565")
    (cons 320 "#7f5e5e")
    (cons 340 "#585858")
    (cons 360 "#585858")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
