;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t -*-
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
;; Things I want to see set before init.el.
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
