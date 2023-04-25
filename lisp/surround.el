;;; surround.el --- Don't leak encrypted files -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Trevor Richards
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
;; A package for adding, removing and swapping surrounding pairs.  Inspired by
;; Tim Pope's vim-surround.
;;
;;; Code:

(defvar surround-pairs '(("{" . "}")
                         ("(" . ")")
                         ("[" . "]")
                         ("<" . ">"))
  "A list of asymmetric pairs for `surround' to respect.")

(defun surround--seek-outer-boundary (start left pair count)
  "Seek out the boundary of an outside `PAIR' from the `START'.
If `LEFT' is non-nil, seek left. Otherwise, seek right."
  (let* ((search (if left #'search-backward #'search-forward))
         (ch-match (if left (car pair) (cdr pair)))
         (ch-skip  (if left (cdr pair) (car pair)))
         (sym (eq ch-skip ch-match))
         (case-fold-search nil))
    (save-excursion
      (if sym
          (apply search (list ch-match nil t count))
        (let* ((match (apply search (list ch-match nil t count)))
               (imbalance (count-matches (regexp-quote ch-skip)
                                         (region-beginning)
                                         (region-end)))
               (mcount (count-matches (regexp-quote ch-match)
                                      (region-beginning)
                                      (region-end))))
          (if (and (>= imbalance mcount) match)
              (surround--seek-outer-boundary start left pair
                                             (+ (- imbalance mcount) 1))
            (deactivate-mark)
            match))))))

(defun surround--seek-bounds (pair)
  "Find the bounds of a surrounding `PAIR' around the point."
  (let ((bounds (cons (surround--seek-outer-boundary (point) t pair 1)
                      (surround--seek-outer-boundary (point) nil pair 1))))
    (if (and (car bounds) (cdr bounds))
        bounds
      (user-error (format "No surrounding pair: %s" pair)))))

(defun surround--add-pair (bounds pair)
  "Add an arbitrary surrounding `PAIR' of chars to a `BOUNDS'."
  (save-excursion
    (goto-char (car bounds))
    (insert (car pair))
    (goto-char (+ (cdr bounds) 1))
    (insert (cdr pair))))

(defun surround--delete-pair (bounds)
  "Delete a surrounding pair outside the `BOUNDS' a range of positions."
  (save-excursion
    (goto-char (- (cdr bounds) 1))
    (delete-char 1)
    (goto-char (car bounds))
    (delete-char 1)))

(defun surround--change-pair (bounds)
  "Swap out an existing `PAIR' outside of `BOUNDS'."
  (let* ((to-what (char-to-string (read-char (message "To new pair: "))))
         (new-pair (or (assoc to-what surround-pairs)
                       (rassoc to-what surround-pairs)
                       (cons to-what to-what))))
    (surround--delete-pair bounds)
    (surround--add-pair (cons (car bounds) (- (cdr bounds) 2)) new-pair)))

(defun surround ()
  "Add surrounding pairs to a region, or change/delete an existing pair.
Inspired by vim-surround."
  (interactive)
  (let* ((case-fold-search nil)
         (method (if (and (region-active-p)
                          (not (eq (region-beginning) (region-end))))
                     ?a
                   (read-char-choice "(c)hange or (d)elete pair? " '(?c ?d))))
         (target (char-to-string (read-char (message "Pair:"))))
         (pair (or (assoc target surround-pairs)
                   (rassoc target surround-pairs)
                   (cons target target)))
         (bounds (or (and
                      (region-active-p)
                      (car (region-bounds)))
                     (surround--seek-bounds pair))))
    (cond ((eq method ?a) (surround--add-pair bounds pair))
          ((eq method ?c) (surround--change-pair bounds))
          ((eq method ?d) (surround--delete-pair bounds)))))

(provide 'surround)
;;; surround.el ends here
