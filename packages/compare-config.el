;;; compare-before-loading.el --- Checks .org config files for updates before babel loading them -*- lexical-binding: t; -*-

;; Copyright (C) Trevor Richards

;; Author: Trevor Richards <trev@trevdev.ca>
;; Version: 0.2.0
;; Keywords: performance, utility
;; URL: https://github.com/trev-dev/compare-before-loading.el

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

;;; Commentary:
;; This package contains a small function library for comparing your .org
;; configuration file against a timestamp of a previous version of that file.
;; If the timestamps do not match, or your config.el is missing, it will use
;; org-babel-tangle-file to create a new config.el.  The stamp is stored
;; in a hidden file that is the same as your config file name with the .prev
;; extension

;;;  Example:
;; (require 'compare-config)
;; If no path is provided the default is "~/.emacs.d/config.org"
;; (compare-config-before-load "/path/to/file.org")

;;; Code:
;;; Global Variables:
(defgroup compare-config nil
  "Compare your config against an old version before tangling it."
  :group 'tools)

(defcustom compare-config-org-file "~/.emacs.d/config.org"
  "The location of your .org formatted configuration file."
  :type 'string
  :group 'compare-config)

(defcustom compare-config-use-temp nil
  "Use the systems temporary directory for storing a config file mtime."
  :type 'boolean
  :group 'compare-config)

(defcustom compare-config-auto-tangle nil
  "Automatically tangle the configuration file on save."
  :type 'boolean
  :group 'compare-config)

;;; Functions:

(defun compare-config-get-mtime (path)
  "Get the modified time of a file from a PATH if it exists."
  (if (file-exists-p (expand-file-name path))
      (current-time-string
       (file-attribute-modification-time
        (file-attributes (expand-file-name path))))))

(defun compare-config-get-config-mtime ()
  "Get the modified time of the org config file, or error."
  (let ((mtime (compare-config-get-mtime compare-config-org-file)))
    (if (not mtime)
        (error (format
                "Missing config Org file: %s"
                (expand-file-name compare-config-org-file))))
    mtime))

(defun compare-config-get-storage-file ()
  "Get a path for where the saved time stamp of an Org config file should be."
  (format
   "%s.%s.prev"
   (if compare-config-use-temp
       temporary-file-directory
     (file-name-directory
      (expand-file-name compare-config-org-file)))
   (file-name-nondirectory (expand-file-name compare-config-org-file))))

(defun compare-config-record-timestamp ()
  "Record a timestamp of the current configuration."
  (with-temp-file (compare-config-get-storage-file)
    (insert (compare-config-get-config-mtime))
    (buffer-string)))

(defun compare-config-get-previous-mtime ()
  "Retrieve the previously saved config modified time.
If it does not exist, make a new timestamp and return that instead."
  (let ((path (compare-config-get-storage-file)))
    (if (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      (compare-config-record-timestamp))))

(defun config-has-changed ()
  "Compare a config file against its previous version using a CONF-PATH."
  (let ((prev-mtime (compare-config-get-previous-mtime))
        (next-mtime (compare-config-get-config-mtime)))
    (if prev-mtime
        (progn
          (message (format "%s" (equal prev-mtime next-mtime)))
          (not (equal prev-mtime next-mtime))))))

(defun compare-config-tangle-config ()
  "Tangle, then create a new timestamp for the org config.
Prompt the user to restart Emacs."
  (interactive)
  (if (not (functionp 'org-babel-tangle-file))
      (require 'org))
  (compare-config-record-timestamp)
  (org-babel-tangle-file compare-config-org-file)
  (message "Your configuration has been tangled. Restart Emacs to use it."))

(defun compare-config-load-config()
  "Load an existing configuration."
  (let* ((name (file-name-sans-extension
               (file-name-nondirectory compare-config-org-file)))
         (dir (file-name-directory compare-config-org-file))
         (path (format "%s%s.el" dir name)))
    (if (file-exists-p path)
        (load-file path)
      (compare-config-tangle-config))))

;;;###autoload
(defun compare-config-before-load (&optional config)
  "Compare your Org `CONFIG' to its previous version before using org babel."
  (if (stringp config)
      (setq compare-config-org-file config))
  (if (config-has-changed)
      (compare-config-tangle-config)
    (compare-config-load-config)))

(provide 'compare-config)
;;; compare-before-loading.el ends here
