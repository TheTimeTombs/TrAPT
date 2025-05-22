;;; trapt-utils.el --- Helper functions for TrAPT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250522
;; Package-Requires: ((emacs "24.3"))
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

;; This file contains helper functions for the TrAPT package.

;;; Code:

(require 'tablist)

(defvar trapt-utils--apt-options '("-o \"apt::color=no\"")
  "A list of options to always pass to APT -o.
These options in these arguments are essential to ensure
proper parsing of the results that are returned from APT.")

(defvar trapt-utils--buffer-names '()
  "A list of tablist buffer for TrAPT list.
This list will be populated with buffer names as the buffers are created.")

(defmacro trapt-utils--check-mode (mode body)
  "BODY is a Lisp form that will execute if the current mode is MODE.
If not, an error will be thrown."
  `(if (string= mode-name ,mode)
       ,body
     (error (format "Error: Function must be called from %s mode." ,mode))))

(defmacro trapt-utils--run-ssh (server body)
  "Run BODY with tramp on SERVER using ssh."
  `(let* ((default-directory (expand-file-name
                              (format "/ssh:%s:~/" ,server))))
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
  "Get the item from index IDX for the current item at point in a tablist."
  (let ((element (tabulated-list-get-entry)))
    (aref element idx)))

(defun trapt-utils--build-command-string (operation
                                          &optional packages arguments)
  "Concatenates the elements of an APT command string.

OPERATION is a string containing a command for the APT package tool.

PACKAGES is list or space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGUMENTS is a list or space-separated string of arguments to the apt command.
If no ARGUMENTS is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (trapt-utils--list-to-string (list "sudo apt"
                                     operation
                                     (trapt-utils--list-or-string packages)
                                     (trapt-utils--list-or-string arguments))))

(defun trapt-utils--vterm-command (command)
  "Opens a new vterm window and insert and execute COMMAND."
  (require 'vterm)
  (vterm-other-window)
  (vterm-clear)
  (vterm-send-string command)
  (vterm-send-return))

(defun trapt-utils--run-command (command &optional shell server)
  "Run COMMAND with `async-shell-command'.

SHELL can be nil, `vterm', or `eshell'. If the value is `vterm' or `eshell',
COMMAND will be run using that shell mode.

If SERVER is in the form username@servername, then the APT command will be run
on that corresponding remote server."
  (message (format "Running: %s" command))
  (cond ((string= shell "vterm")
         (trapt-utils--vterm-exec command server))
        ((string= shell "eshell")
         (trapt-utils--eshell-exec command server))
        (t (async-shell-command command))))

(defun trapt-utils--vterm-exec (command &optional server)
  "Insert the string COMMAND in a vterm buffer and execute it.

If SERVER is a server name of the form username@server, run the
command using ssh."
  (interactive)
  (if server
      (trapt-utils--run-ssh server (trapt-utils--vterm-command command))
    (trapt-utils--vterm-command command)))

(defun trapt-utils--eshell-exec (command &optional server)
  "Run COMMAND in eshell.

If SERVER is a server name of the form username@server, run the
command using ssh."
  (if server
      (trapt-utils--run-ssh server (eshell-command command)))
  (eshell-command command))

(defun trapt-utils--list-to-string (lst)
  "Take a list of arguments LST and return them as a string separated by space."
  (cond ((not lst) nil)
        ((listp lst) (mapconcat #'identity lst " "))
        (t (error "Error: Must be a list!"))))

(defun trapt-utils--shell-command-to-string (command &optional server)
  "Run shell COMMAND in a shell and return the output as string.

SERVER is a string of the form username@server that specifies a server on which
to run the command."
  ;; Add options from `trapt-utils--apt-options' before
  ;; sending the command to ensure the correct string is returned.
  (let ((command-string (concat
                         command
                         " "
                         (trapt-utils--list-to-string
                          trapt-utils--apt-options))))
    (if server
        (progn (message (format "Running: %s on %s" command server))
               (trapt-utils--run-ssh server
                                     (shell-command-to-string command-string)))
      (progn (message (format "Running: %s" command))
             (shell-command-to-string command-string)))))

(provide 'trapt-utils)

;;; trapt-utils.el ends here
