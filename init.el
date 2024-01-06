;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022-2023 Trevor Richards
;;
;; Author: Trevor Richards <trev@trevdev.ca>
;; Maintainer: Trevor Richards <trev@trevdev.ca>
;; URL: https://git.sr.ht/~trevdev/emacs.d
;; Created: 22nd April, 2023
;; Version: 2.0.0
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
;; A hopefully simpler init.el from a more civilized age.
;;
;;; Code:

(defvar emacs-startup-time (current-time)
  "When Emacs started init.el.")

(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/"))
(add-to-list 'load-path (concat (getenv "HOME")
                                "/.local/share/emacs/site-lisp/"))

(require 'td-editor-settings)
(require 'td-commands)
(require 'td-prog-mode)
(require 'td-package-configs)
(require 'td-syntax)
(require 'td-eglot)
(require 'td-org)
(require 'td-denote)

(load-theme 'tangonov t)

(add-hook 'after-init-hook #'td-report-init-time)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(load custom-file 'noerror 'nomessage)

(provide 'init)
;;; init.el ends here
