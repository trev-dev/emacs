;;; doom-material-dark-devel-theme.el --- inspired by Material Theme by xrei -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-material-dark-devel-theme nil
  "Options for the `material dark' theme."
  :group 'doom-themes)


(defcustom doom-material-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-material-dark-devel-theme
  :type '(choice integer boolean))

(def-doom-theme doom-material-dark-devel
  "A darker version of the Material Theme inspired by xrei"

  ;; name        default   256       16
  ((bg         '("#131519" nil       nil))
   (bg-alt     '("#3a404c" nil       nil))
   (base0      '("#171F24" "black"   "black"))
   (base1      '("#262626" "#262626" "brightblack"))
   (base2      '("#303030" "#303030" "brightblack"))
   (base3      '("#3A3A3A" "#3A3A3A" "brightblack"))
   (base4      '("#4a4a4a" "#444444" "brightblack"))
   (base5      '("#585858" "#585858" "brightblack"))
   (base6      '("#626262" "#626262" "brightblack"))
   (base7      '("#767676" "#767676" "brightblack"))
   (base8      '("#A8A8A8" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#f57373" "#ff0000" "red"))
   (orange      '("#F78C6C" "#ff5f00" "brightred"))
   (green       '("#c3e88d" "#afff00" "green"))
   (teal        '("#44b9b1" "#00d7af" "brightgreen"))
   (yellow      '("#ffcb6b" "#ffd700" "brightyellow"))
   (blue        '("#82aaff" "#5fafff" "brightblue"))
   (dark-blue   '("#7986E7" "#d7ffff" "blue"))
   (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   (violet      '("#bb80b3" "#d787af" "magenta"))
   (cyan        '("#89DDFF" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80cbc4" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   base2)
   (selection      base4)
   (builtin        blue)
   (comments       base6)
   (doc-comments   base6)
   (constants      orange)
   (functions      blue)
   (keywords       cyan)
   (methods        blue)
   (operators      cyan)
   (type           magenta)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (doom-lighten bg 0.05))
   (modeline-bg-alt bg)
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-material-padded-modeline
      (if (integerp doom-material-padded-modeline) doom-material-padded-modeline 4))))

  ;;;; Base theme face overrides
  (;;;; emacs
   (lazy-highlight :background (doom-darken green 0.5) :foreground green :weight 'bold)
   (minibuffer-prompt :foreground yellow)
   (region :background (doom-darken dark-cyan 0.5) :foreground dark-cyan :distant-foreground (doom-darken fg 0.2) :extend t)
   (hl-line :background base2 :foreground nil)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (tooltip :background (doom-darken bg-alt 0.2) :foreground fg)
   (cursor :background yellow)
   (line-number-current-line
     :inherit '(hl-line default)
     :foreground cyan :distant-foreground nil
     :weight 'normal :italic nil :underline nil :strike-through nil)
   (completions-first-difference :foreground yellow)
   (icomplete-first-match :foreground green :underline t :weight 'ultrabold)

   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; highlight-thing highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; org <built-in>
   ((org-block &override) :background (doom-lighten bg 0.04))
   ((org-block-background &override) :background (doom-lighten bg 0.04))
   ((org-block-begin-line &override) :background bg :height 0.85)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; ivy
   (ivy-current-match :background base3)
   (ivy-minibuffer-match-face-2
    :inherit 'ivy-minibuffer-match-face-1
    :foreground dark-cyan :background base1 :weight 'semi-bold)
   ;;;; js2-mode
   (js2-jsdoc-tag                :foreground magenta)
   (js2-object-property          :foreground yellow)
   (js2-object-property-access   :foreground cyan)
   (js2-function-param           :foreground violet)
   (js2-jsdoc-type               :foreground base8)
   (js2-jsdoc-value              :foreground yellow)
   (js2-jsdoc-html-tag-delimiter :foreground base8)
   (js2-jsdoc-html-tag-name      :foreground base8)
   ;;;; typescript.el
   (typescript-jsdoc-tag       :foreground magenta)
   (typescript-jsdoc-type      :foreground base8)
   (typescript-jsdoc-value     :foreground cyan)
   ;;;; lsp
   (lsp-headerline-breadcrumb-symbols-face :foreground base7)
   (lsp-ui-sideline-symbol-info :foreground fg-alt :background bg)
   (lsp-ui-doc-background :foreground fg :background bg)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)
   (rjsx-tag-bracket-face :foreground cyan)
   ;;;; Magit
   (magit-header-line :background (doom-lighten modeline-bg 0.2) :foreground green :weight 'bold
                      :box `(:line-width 3 :color ,(doom-lighten modeline-bg 0.2)))
   ;;;; Selectrum
   (selectrum-current-candidate :foreground green :underline green :weight 'ultrabold)
   (selectrum-prescient-primary-highlight :foreground cyan)
   (selectrum-prescient-secondary-highlight :foreground yellow)
   (treemacs-git-modified-face :foreground vc-modified)
   ;;;; Web Mode
   (web-mode-html-tag-face :foreground red)
   (web-mode-html-attr-equal-face :foreground cyan)
   ;;;; Org Mode
   (org-level-1 :foreground green)
   (org-level-2 :foreground yellow)
   (org-level-3 :foreground red)
   (org-level-4 :foreground cyan)
   (org-level-5 :foreground blue)
   (org-level-6 :foreground magenta)
   (org-level-7 :foreground teal)
   (org-level-8 :foreground violet)
   (org-todo :foreground orange)
   (org-drawer :foreground comments :height 0.85)
   (org-meta-line :foreground comments :height 0.85)
   (org-special-keyword :foreground cyan :height 0.85)
   (org-property-value :foreground fg-alt :height 0.85)
   (org-tag :foreground fg-alt :height 0.85)
   (org-document-info-keyword :foreground red :height 0.85)
   (org-document-info :foreground fg-alt :height 0.85)
   (org-document-title :foreground yellow :height 0.85)

   ;;;; css
   (css-property :foreground orange)
   (css-proprietary-property :foreground magenta)
   (css-selector :foreground yellow)
   ;;;; Ledger Mode
   (ledger-occur-xact-face :background nil :weight 'ultrabold)
   ;;;; Evil Googles
   (evil-goggles-default-face :foreground (doom-darken cyan 0.5) :background cyan)
   (evil-goggles-delete-face :foreground (doom-darken red 0.5) :background red)
   (evil-goggles-paste-face :foreground (doom-darken green 0.5) :background green)
   ;;;; Eglot
   (eldoc-highlight-function-argument :weight 'bold :underline t)
   (eglot-highlight-symbol-face :weight 'bold :background base3)
   (eldoc-box-border :background fg-alt)
   ))



;;; Doom-material-theme.el ends here
