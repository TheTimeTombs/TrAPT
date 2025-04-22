;;; trapt-org.el --- Select APT packages with Org mode -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 2025-04-20
;; Package-Requires: (org)
;; Homepage: tbd
;; Keywords: APT org transient


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package is part of `trapt'. This package adds functions that 
;; add the ability to export data from `trapt-list-mode' to an Org
;; mode buffer. The Org export has each packages as a heading. Using
;; the custom TODO headings defined in `trapt-org-todo-keywords'
;; allows package headings to marked for APR operations byt the
;; APT pacakge manager.

;;; Code:


(require 'org)

(defgroup trapt-org nil "")

(defcustom trapt-org-todo-keywords '(("install" . "INSTALL")
                                     ("reinstall" . "REINSTALL")
                                     ("remove" . "REMOVE")
                                     ("purge" . "PURGE")
                                     ("upgrade" . "UPGRADE"))
  "Custom Org mode TODO keywords ."
  :group 'trapt-org
  :type 'alist)

(defcustom trapt-org-done-keyword "DONE"
  "Custom Org mode TODO keyword for terminal state.")

(defvar trapt-org-export-format "* %s %s \n:PROPERTIES:\n:Version: %s\n:Architecture: %s\n:END:\n")

(defun trapt-org--status-to-tags (status)
  "Converts entries from the status column of the APT list buffer or Org mode tags."
  (let* ((tag-string status)
         (tag-string (string-replace "[" ":" tag-string))
         (tag-string (string-replace "]" ":" tag-string))
         (tag-string (string-replace "," ":" tag-string)))
    tag-string))

(defun trapt-org--get-heading-name ()
  "Returns the heading from the current Org heading."
  (nth 4 (org-heading-components)))

(defun trapt-org--export (entries)
  ""
  (trapt-utils--check-mode trapt-list-mode-name
                           (progn
                             (switch-to-buffer (generate-new-buffer "APT List Org Export"))
                             (org-mode)
                             (org-export-insert-default-template 'default)
                             (insert (concat (trapt-org--generate-custom-todos) "\n\n"))
                             (dolist (item entries)
                               (insert (format trapt-org-export-format
                                               (aref (cadr item) 0)
                                               (trapt-org--status-to-tags (aref (cadr item) 4))
                                               (aref (cadr item) 2)
                                               (aref (cadr item) 3)))))))

(defun trapt-org-export-marked ()
  "Export all marked items from the '*APT List*' buffer to a new Org mode buffer.
The format of the Org entries the output format for the org mode is determined by
the variable 'trapt-list-org-export-format'."
  (interactive)
  (trapt-utils--check-mode trapt-list-mode-name
                           (trapt-org--export (tablist-get-marked-items))))

(defun trapt-org-export-all ()
  "Export all items from the '*APT List*' buffer to a new Org mode buffer.
The format of the Org entries the output format for the org mode is determined by
the variable 'trapt-list-org-export-format'."
  (interactive)
  (trapt-utils--check-mode trapt-list-mode-name
                           (trapt-org--export tabulated-list-entries)))

(defun trapt-org--generate-custom-todos ()
  "Generates the custom TODO header for trapt exported Org mode ciles."
  (mapconcat #'identity `("#+TODO:"
                          ,(mapconcat #'cdr
                                      trapt-org-todo-keywords           
                                      " ")
                          "|"
                          ,trapt-org-done-keyword)
             " "))

;;;###autoload 
(defun trapt-org-execute (&opt operation)
  "Runs an APT command from an active Org mode buffer using package names from headlines marked with TODO keywords matching those in `trapt-org-todo-keywords'.

If no OPERATION is given, the user will be prompted for an operation. OPERATION must be a value from `trapt-org-todo-keywords'.

When an operation is chosen, a that APT operation with be executed on the package names in the current org mode buffer marked with the todo keyword from `trapt-org-todo-keywords' that corresponds to the OPERATION value."
  (interactive)
  (let* ((operation (or operation
                        (completing-read "APT operation: "
                                         (mapcar #'car
                                                 trapt-org-todo-keywords)))
                    (tag (cdr (assoc operation trapt-org-todo-keywords)))
                    (package-string  (trapt-utils--list-to-string
                                      (org-map-entries
                                       (lambda ()
                                         (nth 4
                                              (org-heading-components)))
                                       (format "TODO=\"%s\"" tag))))))
    (trapt--execute operation package-string)))

(provide 'trapt-org)

;;; trapt-org.el ends here.
