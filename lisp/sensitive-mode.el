;;; sensitive-mode.el --- Don't leak encrypted files -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Trevor Richards
;;
;; Author: Trevor Richards <trev@trevdev.ca>
;; Maintainer: Trevor Richards <trev@trevdev.ca>
;; URL: TBD
;; Created: 22nd April, 2023
;; Version: 0.0.1
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
;;
;; This file provides a mode that will disable backups and autosaves on-demand.
;; It is meant to be used with sensitive files that are decrypted that you
;; do not want accidently leaked in plain.
;;
;;; Code:

;;;###autoload
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

(provide 'sensitive-mode)
;;; sensitive-mode.el ends here
