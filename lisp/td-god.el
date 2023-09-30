;;; td-god.el --- LSP configuraton -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; My bespoke god-mode configuration.  It includes numerous features:
;;
;; - Seeking with "f" and "t", similar to vim
;; - Insert "after" the character at point. Adapts to the direction the point
;;   moved last
;; - Helpful advice
;;
;;; Code:
(require 'td-helpers)

;;; Functions:
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

;;; Insert Ahead:
;; Provides functionality to drop into insert mode 1 char ahead of the point.
;; The movement "ahead" depends on the direction you are going prior to using
;; `god/insert-ahead'.

(defvar-local god/ahead-direction 1
  "A cached value of the presumed `god/insert-ahead' direction.")

(defun god/set-ahead-direction (&optional dir)
  "Set `god/ahead-direction'. If `DIR' is 1, it's forward.
A value of -1 is backward.'"
  (let ((direction (or dir 1)))
    (unless (= direction god/ahead-direction)
      (setq-local god/ahead-direction direction))))

(defun god/insert-ahead (&rest _args)
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

;;; org-mode advice

(advice-add 'org-meta-return :after
            #'god/exit-god-local
            (function 'god/insert-after-org-meta-return))

(advice-add 'org-insert-todo-heading :after
            #'god/exit-god-local
            (function 'god/insert-after-org-new-heading))

(advice-add 'org-insert-heading-respect-content :after
            #'god/exit-god-local
            (function 'god/insert-after-org-heading-respect-content))

;;; Seek character:
;; This provides a vim-like "f" and "t" option to god mode.

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

(setq cursor-type '(bar . 4))

(defun god/cursor-toggle ()
  "Toggle the cursor between a box and bar while in or out of `god-mode'."
  (setq cursor-type (if (bound-and-true-p god-local-mode)
                        'box
                      '(bar . 4))))

(setq god-mode-alist '((nil . "C-")
                       ("m" . "M-")
                       ("M" . "C-M-")))
(setq god-mode-enable-function-key-translation nil)

;;; Keybinds:

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
                       ("T" . god/seek)
                       ("t" . god/seek-until)
                       ("P" . td-backward-chunk)
                       ("N" . td-forward-chunk)
                       ("(" . kmacro-start-macro)
                       (")" . kmacro-end-or-call-macro)
                       ("{" . backward-paragraph)
                       ("}" . forward-paragraph)
                       ("u" . undo)
                       ("U" . undo-redo)
                       ("q" . quit-window)
                       ("z" . repeat)
                       ("," . er/keymap)))

;;; Apply features:

(with-eval-after-load 'god-mode
  (add-hook 'after-init-hook #'god-mode)
  (require 'god-mode-isearch)
  (dolist (mode '(notmuch-hello-mode
                  notmuch-search-mode
                  notmuch-show-mode
                  vterm-mode))
    (add-to-list 'god-exempt-major-modes mode))

  (add-to-list 'emulation-mode-map-alists
               `((god-local-mode . ,god-local-mode-map)))

  (td-bind-keys god/keybinds god-local-mode-map)

  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

  (god/cursor-toggle)
  (add-hook 'post-command-hook #'god/cursor-toggle)

  (with-eval-after-load 'which-key
    (which-key-enable-god-mode-support)))

(td-bind-keys '(("C-c G"    . god-mode-all)
                ("C-c g"    . god-local-mode) ; Non-graphical fallback.
                ("<escape>" . god-local-mode)))

(provide 'td-god)
;;; td.god.el ends here
