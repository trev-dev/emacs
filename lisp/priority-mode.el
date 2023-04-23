;;; priority-mode.el --- A high priority emulation mode -*- lexical-binding: t -*-
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
;; I hate it when other plugins clobber my bespoke keymaps.  This package creates
;; an emulation mode that should take higher priority over most other minor and
;; major modes.
;;
;;; Code:

;;;###autoload
(define-minor-mode priority-mode
  "A minor mode for short-listing keybindings.
This will prevent other modes form overriding keys that I would prefer to
see bound."
  :init-value nil
  :global t
  :group 'editing
  :keymap (make-sparse-keymap))
(add-to-list 'emulation-mode-map-alists `((priority-mode . ,priority-mode-map)))

(provide 'priority-mode)
;;; priority-mode.el ends here
