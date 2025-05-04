;;; trapt-utils.el --- Helper functions for trapt -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250504
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/tfree87/trapt
;; Keywords: APT


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

;; This file contains helper functions for the TrAPT package.

;;; Code:

(defmacro trapt-utils--check-mode (mode &body)
  "BODY is a Lisp form that will execute if the current mode is MODE.
If not, an error will be thrown."
  `(if (string= mode-name ,mode)
       ,&body
     (error (format "Error: Function must be called from %s mode." ,mode))))

(defun trapt-utils--list-or-string (value)
  "Return VALUE if VALUE is a string.
If VALUE is a list, return a space-separated string."
  (cond ((not value)
         "")
        ((stringp value)
         value)
        ((listp value)
         (trapt-utils--list-to-string value))))

(defun trapt-utils--get-tablist-item (idx)
  "Get the item from index IDX for the current item at point in a tablist buffer."
  (let ((element (tabulated-list-get-entry)))
    (aref element idx)))

(defun trapt-utils--build-command-string (operation &optional packages arguments)
  "Concatenates the elements of an APT command string.

OPERATION is a string containing a command for the APT package tool.

PACKAGES is list or space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGUMENTS is a list or space-separated string of arguments to the apt command.
If no ARGUMENTS is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (trapt-utils--list-to-string (list "sudo"
                                     "apt"
                                     operation
                                     (trapt-utils--list-or-string packages)
                                     (trapt-utils--list-or-string arguments))))

(defun trapt-utils--vterm-exec (command)
  "Insert the string COMMAND in a vterm buffer and execute it."
  (interactive)
  (require 'vterm)
  (vterm-other-window)
  (vterm-clear)
  (vterm-send-string command)
  (vterm-send-return))

(defun trapt-utils--eshell-exec (command)
  "Run COMMAND in eshell."
  (eshell-command command))

(defun trapt-utils--list-to-string (lst)
  "Take a list of arguments LST and return them as a string separated by space."
  (cond ((not lst) nil)
        ((listp lst) (mapconcat #'identity lst " "))
        (t (error "Error: Must be a list!"))))

(defun trapt-utils--shell-command-to-string (command)
  "Run shell COMMAND in a shell and return the output as string."
  (let ((command-output (shell-command-to-string command)))
    (message (concat "Running: " command))
    command-output))

(provide 'trapt-utils)

;;; trapt-utils.el ends here
