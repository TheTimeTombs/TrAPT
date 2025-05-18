;;; trapt-utils.el --- Helper functions for trapt -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250508
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

(require 'tablist)

(defcustom trapt-remotes '()
  "A list of remote ssh connections for TrAPT.
`trapt-remotes' should be a quoted list in which each element of the list is in
the form username@server."
  :type '(repeat string)
  :group 'TrAPT)

(defvar trapt-utils--apt-options '("-o \"apt::color=no\"")
  "A list of options to always pass to APT -o.
These options in these arguments are essential to ensure
proper parsing of the results that are returned from APT.")

(defmacro trapt-utils--check-mode (mode body)
  "BODY is a Lisp form that will execute if the current mode is MODE.
If not, an error will be thrown."
  `(if (string= mode-name ,mode)
       ,body
     (error (format "Error: Function must be called from %s mode." ,mode))))

(defmacro trapt-utils--run-ssh (remote body)
  "Run BODY with tramp using REMOTE server."
  `(let* ((default-directory (expand-file-name
                              (format "/ssh:%s:~/" ,remote))))
     (with-connection-local-variables
      ,body)))

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

(defun trapt-utils--vterm-exec (command &optional remote)
  "Insert the string COMMAND in a vterm buffer and execute it.

If REMOTE is a server name of the form username@server, run the
command using ssh."
  (interactive)
  (require 'vterm)
  (if remote
      (let ((server (completing-read "Select remote: " trapt-remotes)))
        (trapt-utils--run-ssh server
                              (progn (vterm-other-window)
                                     (vterm-clear)
                                     (vterm-send-string command)
                                     (vterm-send-return))))
    (progn (vterm-other-window)
           (vterm-clear)
           (vterm-send-string command)
           (vterm-send-return))))

(defun trapt-utils--eshell-exec (command &optional remote)
  "Run COMMAND in eshell.

If REMOTE is a server name of the form username@server, run the
command using ssh."
  (if remote
      (let ((server (completing-read "Select remote: " trapt-remotes)))
        (trapt-utils--run-ssh server (eshell-command command)))
    (eshell-command command)))

(defun trapt-utils--list-to-string (lst)
  "Take a list of arguments LST and return them as a string separated by space."
  (cond ((not lst) nil)
        ((listp lst) (mapconcat #'identity lst " "))
        (t (error "Error: Must be a list!"))))

(defun trapt-utils--shell-command-to-string (command &optional remote)
  "Run shell COMMAND in a shell and return the output as string.

REMOTE is a string of the form username@server that specifies a server on which
to run the command."
  ;; Add options from `trapt-utils--apt-options' before
  ;; sending the command to ensure the correct string is returned.
  (let ((command-string (concat
                         command
                         " "
                         (trapt-utils--list-to-string trapt-utils--apt-options))))
    (if remote
        (let ((server (completing-read "Select remote: " trapt-remotes)))
          (message (format "Running: %s on %s" command server))
          (trapt-utils--run-ssh server
                                (shell-command-to-string command-string)))
      (progn (message (format "Running: %s" command))
             (shell-command-to-string command-string)))))

(provide 'trapt-utils)

;;; trapt-utils.el ends here
