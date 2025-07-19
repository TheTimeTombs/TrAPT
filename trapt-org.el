;;; trapt-org.el --- Manage APT with Org mode -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250522
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/tfree87/trapt
;; Keywords: processes


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
(require 'tablist)
(require 'trapt-list)
(require 'trapt-utils)

(defgroup trapt-org nil "Customization options for TrAPT-Org."
  :group 'trapt
  :prefix "trapt-org-")

(defvar trapt-org--buffer-name "APT List Org Export"
  "The name of the buffer after exporting `trapt-list' to Org.")

(defcustom trapt-org-todo-keywords '(("full-upgrade" . "FULL-UPGRADE")
                                     ("install" . "INSTALL")
                                     ("reinstall" . "REINSTALL")
                                     ("remove" . "REMOVE")
                                     ("purge" . "PURGE")
                                     ("upgrade" . "UPGRADE"))
  "Custom Org mode TODO keywords ."
  :group 'TrAPT-Org
  :type '(repeat alist))

(defcustom trapt-org-done-keyword "DONE"
  "Custom Org mode TODO keyword for terminal state."
  :type '(string))

(defvar trapt-org-export-format
  "* %s %s \n:PROPERTIES:\n:Version: %s\n:Architecture: %s\n:END:\n"
  "The format of the Org properties drawer.
This is used when exporting results with `trapt-org--export'")

(defun trapt-org--status-to-tags (status)
  "Convert status column of the APT list buffer to Org mode tag.

STATUS is a string from the status column of APT list."
  (let* ((tag-string status)
         (tag-string (string-replace "[" ":" tag-string))
         (tag-string (string-replace "]" ":" tag-string))
         (tag-string (string-replace "," ":" tag-string)))
    tag-string))

(defun trapt-org--get-heading-name ()
  "Return the heading from the current Org heading."
  (nth 4 (org-heading-components)))

(defun trapt-org--init-buffer ()
  "Create a new Org buffer and add the default header."
  (switch-to-buffer (generate-new-buffer
                     trapt-org--buffer-name))
  (org-mode)
  (org-export-insert-default-template 'default)
  (insert (concat (trapt-org--generate-custom-todos)
                  "\n\n")))

(defun trapt-org-export-marked ()
  "Export all marked items from the APT List buffer to Org mode.
The format of the Org entries the output format for the org mode is determined
by the variable `trapt-list-org-export-format'."
  (interactive)
  (let ((entries (tablist-get-marked-items)))
    (trapt-utils--check-mode
     trapt-list--mode-name
     (cl-loop for item in entries
              initially (trapt-org--init-buffer)
              do (insert (format trapt-org-export-format
                                 (aref (cdr item) 0)
                                 (trapt-org--status-to-tags
                                  (aref (cdr item) 4))
                                 (aref (cdr item) 2)
                                 (aref (cdr item) 3)))
              finally (org-mode-restart)))))

(defun trapt-org-export-all ()
  "Export all items from the APT List buffer to Org mode.
The format of the Org entries the output format for the org mode is determined
by the variable `trapt-list-org-export-format'."
  (interactive)
  (let ((entries tabulated-list-entries))
    (trapt-utils--check-mode
     trapt-list--mode-name
     (cl-loop for item in entries
              initially (trapt-org--init-buffer)
              do (insert (format trapt-org-export-format
                                 (aref (cadr item) 0)
                                 (trapt-org--status-to-tags
                                  (aref (cadr item) 4))
                                 (aref (cadr item) 2)
                                 (aref (cadr item) 3)))
              finally (org-mode-restart)))))

(defun trapt-org--generate-custom-todos ()
  "Generate custom TODO header for trapt exported Org mode files."
  (format "#+TODO: %s | %s"
          (mapconcat #'cdr trapt-org-todo-keywords " ")
          trapt-org-done-keyword))

;;;###autoload
(defun trapt-org-execute ()
  "Run an APT command from an active Org mode buffer.

Package names in the Org buffer should be headlines marked with TODO keywords
matching those in `trapt-org-todo-keywords'.

When an operation is chosen, a that APT operation with be executed on the
package names in the current org mode buffer marked with the todo keyword from
`trapt-org-todo-keywords' that corresponds to the OPERATION value."
  (interactive)
  (trapt-utils--check-mode
   "Org"
   (let* ((operation (completing-read "APT operation: "
                                      (mapcar #'car
                                              trapt-org-todo-keywords)))
          (tag (cdr (assoc operation trapt-org-todo-keywords)))
          (packages (trapt-utils--list-to-string
                     (org-map-entries
                      (lambda ()
                        (nth 4
                             (org-heading-components)))
                      (format "TODO=\"%s\"" tag))))
          (shell (bound-and-true-p trapt-shell))
          (command (trapt-utils--build-command-string operation
                                                      packages)))
     (trapt-utils--run-command command shell))))

(provide 'trapt-org)

;;; trapt-org.el ends here
