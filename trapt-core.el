;;; trapt-core.el --- Core functions for TrAPT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250504
;; Package-Requires: (dependencies)
;; Homepage: 
;; Keywords: keywords


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

;; commentary

;;; Code:

(require 'tablist)
(require 'trapt-utils)

(defcustom trapt-shell nil
  "The shell to run TrAPT commands."
  :options '("default" "eshell" "vterm")
  :group 'TrAPT
  :type 'String)

(defvar trapt--marked-packages nil
  "A list of marked packages from one of `trapt--buffer-names'.")

(defvar trapt--buffer-names nil
  "A list of tablist buffer for TrAPT list.
This list will be populated with buffer names as the buffers are created.")

(defun trapt--update-pkg-lst (bufname)
  "Update list variable LSTNAME with marked packages if buffer is BUFNAME."
  (when (get-buffer bufname)
    (progn (switch-to-buffer bufname)
           (when (caar (tablist-get-marked-items))
             (cl-loop for item in (tablist-get-marked-items)
                      collect (aref (cdr item) 0))))))

(defun trapt--run-command (command)
  "Run COMMAND with `async-shell-command'."
  (message (format "Running: %s" command))
  (cond ((string= trapt-shell "vterm")
         (trapt-utils--vterm-exec command))
        (t (async-shell-command command))))

(cl-defun trapt--execute (operation &key packages arglist (prompt t))
  "Run an APT command from an inferior shell.

OPERATION must be a string and can be any command understood by the APT
package manager. Including full-upgrade, install, reinstall, purge, remove,
update, and upgrade.

PACKAGES is list or space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a list or space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given."
  (when (member (buffer-name) trapt--buffer-names)
    (setf trapt--marked-packages (trapt--update-pkg-lst (buffer-name))))
  (let* ((packages (or packages
                       (trapt-utils--list-to-string
                        trapt--marked-packages)
                       (when prompt
                         (read-string
                          (format
                           "Enter packages to %s (space separarted -- enter for all): "
                           operation)))
                       ""))
         (arguments (or arglist
                        (when prompt
                          (read-string (format
                                        "Enter apt %s arguments (space separated -- enter for none): "
                                        operation)))
                        ""))
         (command (trapt-utils--build-command-string
                   operation
                   packages
                   arguments)))
    (if (string= operation "list")
        ;; TODO run this asynchronously or speed up for large lists
        (trapt-utils--shell-command-to-string command)
      (trapt--run-command command))))

;;;###autoload
(defun trapt-apt-install (&optional packages arglist)
  "Run apt install. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "install" :packages packages :arglist arglist))

(defun trapt-apt-reinstall (&optional packages arglist)
  "Run apt reinstall. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "reinstall" :packages packages :arglist arglist))

;;;###autoload
(defun trapt-apt-remove (&optional packages arglist)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "remove" :packages packages :arglist arglist))

;;;###autoload
(defun trapt-apt-upgrade (&optional arglist)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "upgrade" :prompt nil :arlist arglist))

;;;###autoload
(defun trapt-apt-purge (&optional packages arglist)
  "Run apt purge. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "purge" :packages packages :arglist arglist))

;;;###autoload
(defun trapt-apt-autoremove (&optional arglist)
  "Run apt autoremove.This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "autoremove" :arglist arglist :prompt nil))

;;;###autoload
(defun trapt-apt-full-upgrade (&optional arglist)
  "Run apt full-upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "full-upgrade" :prompt nil :arglist arglist))

;;;###autoload
(defun trapt-apt-autoclean ()
  "Run apt autoclean.  This is a wrapper function for `trapt--execute'."
  (interactive)
  (trapt--execute "autoclean" :prompt nil))

;;;###autoload
(defun trapt-apt-update ()
  "Run apt update. This is a wrapper function for `trapt--execute'."
  (interactive)
  (trapt--execute "update" :prompt nil))

(provide 'trapt-core)

;;; trapt-core.el ends here
