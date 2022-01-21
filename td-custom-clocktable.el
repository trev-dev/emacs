(defgroup td/org-mode nil
  "A custom group for trevdev's org mode customizations")
(defcustom td/billable-rate 65
  "The billable rate for calculating 'td/custom-clocktable"
  :type `integer
  :options '(50, 65, 75, 80)
  :group 'td/org-mode)

(defun td/custom-clocktable-indent (level)
  "Create an indent based on org LEVEL"
  (if (= level 1) ""
    (concat (make-string (1- level) ?—) " ")
    ))

(defun td/custom-clocktable-get-prop (key props)
  "Get a specific value using a KEY from a list of PROPS"
  (cdr (assoc key props)))

(defun td/minutes-to-billable (minutes)
  "Get the amount in dollers that a number of MINUTES is worth"
  (let* ((hours (/ (round (* (/ minutes 60.0) 100)) 100.0))
         (amount (* hours td/billable-rate))
         (billable (/ (round (* amount 100)) 100.0)))
    billable))

(defun td/emph-str (string &optional emph)
  "Emphasize a STRING if EMPH is set"
  (if emph
      (format "*%s*" string)
    string))

(defun td/custom-clocktable (ipos tables params)
  "An attempt to clock my voltage time, my way"
  (let* ((lang (or (plist-get params :lang) "en"))
         (block (plist-get params :block))
         (emph (plist-get params :emphasize))
         (header (plist-get params :header))
         (properties (or (plist-get params :properties) '()))
         (comments-on (member "Comment" properties))
         (formula (plist-get params :formula))
         (has-formula (cond ((and formula (stringp formula))
                             t)
                            (formula (user-error "Invalid :formula param"))))
         (effort-on (member "Effort" properties)))
    (goto-char ipos)

    (insert-before-markers
     (or header
         ;; Format the standard header.
         (format "#+CAPTION: %s %s%s\n"
                 (org-clock--translate "Clock summary at" lang)
                 (format-time-string (org-time-stamp-format t t))
                 (if block
                     (let ((range-text
                            (nth 2 (org-clock-special-range
                                    block nil t
                                    (plist-get params :wstart)
                                    (plist-get params :mstart)))))
                       (format ", for %s." range-text))
                   "")))
     "| Task " (if effort-on "| Est" "")
     "| Time | Billable"
     (if comments-on "| Comment" "") "\n")
    (let '(total-time (apply #'+ (mapcar #'cadr tables)))
      (when (and total-time (> total-time 0))
        (pcase-dolist (`(, file-name , file-time , entries) tables)
          (when (and file-time (> file-time 0))
            (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
              (insert-before-markers
               (if (= level 1) "|-\n|" "|")
               (td/custom-clocktable-indent level)
               (concat (td/emph-str headline (and emph (= level 1))) "|")
               (if-let* (effort-on
                         (eft (td/custom-clocktable-get-prop "Effort" props))
                         (formatted-eft (org-duration-from-minutes
                                         (org-duration-to-minutes eft))))
                   (concat (td/emph-str formatted-eft (and emph (= level 1)))
                           "|")
                 (if effort-on "|"
                   ""))
               (concat (td/emph-str
                        (org-duration-from-minutes time)
                        (and emph (= level 1))) "|")
               (concat (td/emph-str
                        (format "$%.2f" (td/minutes-to-billable time))
                        (and emph (= level 1))) "|")
               (if-let* (comments-on
                         (comment
                          (td/custom-clocktable-get-prop "Comment" props)))
                   (concat comment "\n")
                 "\n")))))
        (let ((cols-adjust
               (if (member "Effort" properties)
                   2
                 1)))
          (insert-before-markers
           (concat "|-\n| "
                   (td/emph-str "Totals" emph)
                   (make-string cols-adjust ?|))
           (concat (td/emph-str
                    (format "%s" (org-duration-from-minutes total-time)) emph)
                   "|")
           (concat (td/emph-str
                    (format "$%.2f" (td/minutes-to-billable total-time))
                    emph) "|" ))
          (when has-formula
            (insert "\n#+TBLFM: " formula)))))
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when has-formula (org-table-recalculate 'all))))

(defun td/clocktable-format-toggle ()
  (interactive)
  (if (equal org-duration-format '((special . h:mm)))
      (setq-local org-duration-format '(("h" . nil) (special . 2)))
    (setq-local org-duration-format '((special . h:mm))))
  (org-ctrl-c-ctrl-c))
