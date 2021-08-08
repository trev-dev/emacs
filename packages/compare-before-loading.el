;;; compare-before-loading.el --- Checks .org config files for updates before babel loading them -*- lexical-binding: t; -*-

;; Copyright (C) Trevor Richards

;; Author: Trevor Richards <trev@trevdev.ca>
;; Version: 0.1.0
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
;; org-babel-load-file to create and load a new config.el.  The stamp is stored
;; in a hidden file that is the same as your config file name with the .prev
;; extension

;;;  Example:
;; (require 'compare-before-loading)
;; If no path is provided the default is "~/.emacs.d/config.org"
;; (compare-before-loading "/path/to/file.org")

;;; Code:
(require 'org)

;;; Global Variables:
(defgroup compare-config nil
  "Compare your config against an old version before babel-loading it"
  :group 'tools)

(defcustom path-to-org-config "~/.emacs.d/config.org"
  "The location of your .org formatted configuration file."
  :type 'string
  :group 'compare-config)

(defcustom use-temp-for-config-mtime nil
  "Use the systems temporary directory for storing a config file mtime."
  :type 'boolean
  :group 'compare-config)

;;; Functions:

(defun get-file-mtime-string (path)
  "Get the modified time of a file from a PATH if it exists."
  (if (file-exists-p path)
      (current-time-string
       (file-attribute-modification-time
        (file-attributes path)))))

(defun get-current-config-mtime()
  "Get the modified time of the Org config file."
  (if (file-exists-p path-to-org-config)
      (get-file-mtime-string path-to-org-config)
    (error (format
            "Missing config Org file: %s"
            (expand-file-name path-to-org-config))))
  )

(defun get-stamp-storage-path ()
  "Get a path for where the saved time stamp of an Org config file should be."
  (format
   "%s.%s.prev"
   (if use-temp-for-config-mtime
       temporary-file-directory
     (file-name-directory
      (expand-file-name path-to-org-config)))
   (file-name-nondirectory (expand-file-name path-to-org-config))))

(defun get-previous-config-mtime()
  "Retrieve the previously saved config modified time."
  (let ((path (get-stamp-storage-path)))
    (if (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string)))))

(defun config-has-changed ()
  "Compare a config file against its previous version using a CONF-PATH.
Returns a boolean value for whether or not it has changed."
  (let ((prev-mtime (get-previous-config-mtime))
        (next-mtime (get-current-config-mtime)))
    (message (format "%s" (equal prev-mtime next-mtime)))
    (not (equal prev-mtime next-mtime))))

(defun stamp-and-load-config()
  "Create a timestamp file for the current Org config file then load it."
  (let ((dest (get-stamp-storage-path)))
    (with-temp-file dest (insert (get-current-config-mtime)))
    (message (format "Recorded config timestamp to %s" dest))
    (org-babel-load-file path-to-org-config)))

(defun load-existing-config()
  "Load the existing untangled .org config as it should be unchanged.
If it does not exist, do `org-babel-load-file' on the config Org file"
  (let* ((name (file-name-sans-extension
               (file-name-nondirectory path-to-org-config)))
         (dir (file-name-directory path-to-org-config))
         (path (format "%s%s.el" dir name)))
    (if (file-exists-p path)
        (load-file path)
      (org-babel-load-file path-to-org-config))))

;;;###autoload
(defun compare-before-loading ()
  "Compare your Org config to its prvious version before using org babel."
  (if (config-has-changed)
      (stamp-and-load-config)
    (load-existing-config)))

(provide 'compare-before-loading)
;;; compare-before-loading.el ends here
