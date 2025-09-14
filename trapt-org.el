;;; trapt-org.el --- Manage APT with Org mode -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 1.2
;; Package-Requires: ((emacs "28.1") (tablist))
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

(defgroup trapt-org nil "Customization options for TrAPT-Org."
  :group 'trapt
  :prefix "trapt-org-")

(defvar trapt-org--buffer-name "APT List Org Export"
  "The name of the buffer after exporting `trapt-list' to Org.")

(defcustom trapt-org-todo-keywords '(("FULL-UPGRADE" . trapt-apt-full-upgrade)
                                     ("BUILD-DEP" . trapt-apt-build-dep)
                                     ("INSTALL" . trapt-apt-install)
                                     ("REINSTALL" . trapt-apt-reinstall)
                                     ("REMOVE" . trapt-apt-remove)
                                     ("PURGE" . trapt-apt-purge)
                                     ("UPGRADE" . trapt-apt-upgrade))
  "Custom Org mode TODO keywords for Org mode buffers with APT pacckages.
The TODO keywords are given as an alist in the form of '(keyword . function)
where keyword is a string representing the Org TODO keyword and the function is
an unquoted symbol for the `trapt-apt' function to call for entries with the
keyword when `trapt-org-execute' is called."
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

(defun trapt-org--apt->org (entries)
  "Format and insert`trapt-apt-list' ENTRIES into a new Org buffer."
  (cl-loop for item in entries
           initially (trapt-org--init-buffer)
           do (insert (format trapt-org-export-format
                              (aref (cadr item) 0)
                              (trapt-org--status-to-tags
                               (aref (cadr item) 4))
                              (aref (cadr item) 2)
                              (aref (cadr item) 3)))
           finally (org-mode-restart)))

(defun trapt-org-export-marked ()
  "Export all marked items from the APT List buffer to Org mode.
The format of the Org entries the output format for the org mode is determined
by the variable `trapt-list-org-export-format'."
  (interactive)
  (when (string= (buffer-name) trapt-apt-list-buffer-name)
    (let* ((names (mapcar (lambda (item) (car item)) (bui-list-get-marked)))
           (entries (mapcar (lambda (name) (assoc name tabulated-list-entries)) names)))
      (trapt-org--apt->org entries))))

(defun trapt-org-export-all ()
  "Export all items from the APT List buffer to Org mode.
The format of the Org entries the output format for the org mode is determined
by the variable `trapt-list-org-export-format'."
  (interactive)
  (when (string= (buffer-name) trapt-apt-list-buffer-name)
    (let ((entries tabulated-list-entries))
      (trapt-org--apt->org entries))))

(defun trapt-org--generate-custom-todos ()
  "Generate custom TODO header for trapt exported Org mode files."
  (format "#+TODO: %s | %s"
          (mapconcat #'car trapt-org-todo-keywords " ")
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
  (let* ((tag (completing-read "APT operation: "
                               (mapcar #'car
                                       trapt-org-todo-keywords)))
         
         (function (cdr (assoc tag trapt-org-todo-keywords)))
         (packages (mapconcat (lambda (item) (concat item " "))
                              (org-map-entries
                               (lambda ()
                                 (nth 4
                                      (org-heading-components)))
                               (format "TODO=\"%s\"" tag)))))
    (eval `(,function :packages ,packages :arglist ""))))

(provide 'trapt-org)

;;; trapt-org.el ends here
