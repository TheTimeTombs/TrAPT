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


(autoload 'trapt-utils--vterm-command "vterm")


(defvar trapt-utils--apt-options '("-o \"apt::color=no\"")
  "A list of options to always pass to APT -o.
These options in these arguments are essential to ensure
proper parsing of the results that are returned from APT.")

(defvar trapt-utils--buffer-names '()
  "A list of tablist buffer for TrAPT list.
This list will be populated with buffer names as the buffers are created.")

(defvar trapt-utils--persistent-stats '(trapt-list--num-installed
                                        trapt-list--num-upgradable
                                        trapt-list--num-residual
                                        trapt-list--num-auto-installed)
  "List of statisitics variables to save to disk.")

(defmacro trapt-utils--check-mode (mode body)
  "BODY is a Lisp form that will execute if the current mode is MODE.
If not, an error will be thrown."
  `(if (string= mode-name ,mode)
       ,body
     (error (format "Error: Function must be called from %s mode." ,mode))))

(defmacro trapt-utils--run-ssh (host body)
  "Run BODY with tramp on HOST using ssh."
  `(let* ((default-directory (expand-file-name
                              (format "/ssh:%s:~/" ,host))))
     (with-connection-local-variables
      ,body)))

(defun trapt-utils--set-save-stats (stats-list)
  "Set satistics in STATS-LIST and save to file.

STATS-LIST must of a list in the form of ((var1 . value1)...)."
  (with-temp-file trapt-stats-file
    (insert (format "%s"
                    (cl-loop for element in stats-list
                             do (setf varname (car element))
                             (setf variable-value (cdr element))
                             (set `,varname variable-value)
                             collect `(,varname . ,variable-value))))))

(defun trapt-utils--read-file (filepath)
  "Read contents from FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath)
    (read (buffer-string))))

(defun trapt--remotehost-p (test-string)
  "Return TEST-STRING if it refers to a remote host, otherwise nil."
  (unless (or (eq (type-of test-string) 'string)
              (eq test-string nil))
    (error "Argument must be of string type or nil."))
  (cond ((eq nil test-string) nil)
        ((string-match "localhost" test-string) nil)
        ((string-match "127\.0\.0\.1" test-string) nil)
        (t test-string)))

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
                                          &optional
                                          sudo
                                          packages
                                          arguments)
  "Concatenates the elements of an APT command string.

OPERATION is a string containing a command for the APT package tool.

If SUDO is non-nil then the command will be called with `sudo'.

PACKAGES is list or space-separated string of packages to upgrade. If no
PACKAGES are passed, then the user will be prompted for a space-separated string
containing the list of packages to upgrade.

ARGUMENTS is a list or space-separated string of arguments to the apt command.
If no ARGUMENTS is passed, then the user will be prompted for a space-separated
string containing the list of arguments to pass."
  (let ((package-string (trapt-utils--list-or-string packages))
        (arg-string (trapt-utils--list-or-string arguments)))
    (format "%s%s %s %s%s"
            (if sudo
                "sudo "
              "")
            "apt"
            operation
            (if (not (string= package-string ""))
                (concat package-string " ")
              "")
            arg-string)))

(defun trapt-utils--vterm-command (command)
  "Opens a new vterm window and insert and execute COMMAND."
  (vterm-other-window)
  (vterm-clear)
  (vterm-send-string command)
  (vterm-send-return))

(defun trapt-utils--command (command &optional host)
  "Run COMMAND with `async-shell-command'.

If HOST is a host name of the form username@host, run the
command using ssh."
  (interactive)
  (if (trapt--remotehost-p host)
      (trapt-utils--run-ssh host (async-shell-command command))
    (async-shell-command command)))

(defun trapt-utils--run-command (command &optional shell host)
"Run COMMAND with `async-shell-command'.

SHELL can be nil, `vterm', or `eshell'. If the value is `vterm' or `eshell',
COMMAND will be run using that shell mode.

If HOST is in the form username@hostname, then the APT command will be run
on that corresponding remote host."
(message "Running: %s" command)
(cond ((string= shell "vterm")
       (trapt-utils--vterm-exec command host))
      ((string= shell "eshell")
       (trapt-utils--eshell-exec command host))
      (t (trapt-utils--command command host))))

(defun trapt-utils--vterm-exec (command &optional host)
  "Insert the string COMMAND in a vterm buffer and execute it.

If HOST is a host name of the form username@host, run the
command using ssh."
  (interactive)
  (if (trapt--remotehost-p host)
      (trapt-utils--run-ssh host (trapt-utils--vterm-command command))
    (trapt-utils--vterm-command command)))

(defun trapt-utils--eshell-exec (command &optional host)
  "Run COMMAND in eshell.

If HOST is a host name of the form username@host, run the
command using ssh."
  (if (trapt--remotehost-p host)
      (trapt-utils--run-ssh host (eshell-command command)))
  (eshell-command command))

(defun trapt-utils--list-to-string (lst)
  "Take a list of arguments LST and return them as a string separated by space."
  (cond ((not lst) nil)
        ((listp lst) (mapconcat #'identity lst " "))
        (t (error "Error: Must be a list!"))))

(defun trapt-utils--shell-command-to-string (command &optional host)
  "Run shell COMMAND in a shell and return the output as string.

HOST is a string of the form username@host that specifies a host on which
to run the command."
  ;; Add options from `trapt-utils--apt-options' before
  ;; sending the command to ensure the correct string is returned.
  (let ((command-string (concat
                         command
                         " "
                         (trapt-utils--list-to-string
                          trapt-utils--apt-options))))
    (if (trapt--remotehost-p host)
        (progn (message "Running: %s on %s" command host)
               (trapt-utils--run-ssh host
                                     (shell-command-to-string command-string)))
      (progn (message "Running: %s" command)
             (shell-command-to-string command-string)))))

(provide 'trapt-utils)

;;; trapt-utils.el ends here
