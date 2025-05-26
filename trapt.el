;;; trapt.el --- Emacs interface to APT -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(require 'transient)
(require 'tablist)
(require 'trapt-utils)
(require 'trapt-exec-find)
(require 'trapt-list)
(require 'trapt-org)

(defgroup trapt nil "Customization options for TrAPT."
  :group 'Processes
  :prefix "trapt-")

(defcustom trapt-remotes '()
  "A list of remote ssh connections for TrAPT.
`trapt-remotes' should be a quoted list in which each element of the list is in
the form username@server."
  :type '(repeat string)
  :group 'trapt)

(defcustom trapt-shell nil
  "The shell to run TrAPT commands."
  :type '(string)
  :options '("default" "eshell" "vterm")
  :group 'trapt)

(defvar trapt--tablist-buffers '()
  "A list of tablist buffer names with package tablists for TrAPT.")

(defun trapt--transient-remote (remote)
  "Wrapper function for `trapt--server'.

If the current function was called by a transient menu, call
`trapt--get-server' while passing a non-nil value if `remote' is in the list of
tranisent aruments as determined by `tranisent-args'

Otherwise call `trapt--get-server' passing the value REMOTE."
  (if (bound-and-true-p transient-current-command)
      (let ((arguments (transient-args transient-current-command)))
        ;; Check to see if user called `remote' argument
        (trapt--get-server (member "remote" arguments)))
    (trapt--get-server remote)))

(defun trapt--transient-args-clean (arglist)
  "Remove the value `remote' from ARGLIST and return it."
  (if (bound-and-true-p transient-current-command)
      (let* ((arguments (transient-args transient-current-command))
             ;; Remove `remote' arugment if it is present
             ;; Send empty string if list is empty
             (transient-args (or (cl-remove-if #'(lambda (elt)
                                                   (string= "remote" elt))
                                               arguments)
                                 "")))
        transient-args)
    arglist))

(defun trapt--get-server (remote)
  "Prompt the user for a server name if REMOTE is non-nil."
  (if remote
      (completing-read "Select remote: " trapt-remotes)
    nil))

(defun trapt--get-marked-packages (packages)
  "Return a list of pacakges from a buffer in `trapt--tablist-buffers'.

If the current buffer name is not in `trapt--trablist-buffers', return
PACKAGES."
  (if (member (buffer-name) trapt--tablist-buffers)
      (when (caar (tablist-get-marked-items))
        (cl-loop for item in (tablist-get-marked-items)
                 collect (aref (cdr item) 0)))
    packages))

(cl-defun trapt--execute (operation &key packages arglist (prompt t) server)
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
if none are given.

If SERVER is in the form username@servername, then the APT command will be run
on that corresponding remote server."
  (let* ((packages (or packages
                       (when prompt
                         (read-string
                          (format
                           "Enter packages to %s (space separated): "
                           operation)))
                       ""))
         (arguments (or arglist
                        (when prompt
                          (read-string (format
                                        "Enter apt %s arguments\
 (space separated): "
                                        operation)))
                        ""))
         (command (trapt-utils--build-command-string
                   operation
                   packages
                   arguments)))
    (if (string= operation "list")
        (trapt-list--create-tablist command)
      (trapt-utils--run-command command trapt-shell server))))

;;;###autoload
(cl-defun trapt-apt-install (&key packages arglist (prompt t) remote)
  "Run apt install. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade. If no PACKAGES are
passed, then the user will be prompted for a space-separated string containing
the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "install"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-reinstall (&key packages arglist (prompt t) remote)
  "Run apt reinstall. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade. If no PACKAGES are
passed, then the user will be prompted for a space-separated string containing
the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "reinstall"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-remove (&key packages arglist (prompt t) remote)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade. If no PACKAGES are
passed, then the user will be prompted for a space-separated string containing
the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "remove"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-upgrade (&key arglist prompt remote)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is non-nil, then the user will be prompted for packages and arguments
if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "upgrade"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-purge (&key packages arglist (prompt t) remote)
  "Run apt purge. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade. If no PACKAGES are
passed, then the user will be prompted for a space-separated string containing
the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "purge"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-autoremove (&key arglist prompt remote)
  "Run apt autoremove.This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is non-nil, then the user will be prompted for packages and arguments
if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "autoremove"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-full-upgrade (&key arglist prompt remote)
  "Run apt full-upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "full-upgrade"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-autoclean (&key arglist prompt remote)
  "Run apt autoclean.  This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "autoclean"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-update (&key arglist prompt remote)
  "Run apt update. This is a wrapper function for `trapt--execute'.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
 (trapt--execute "update"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

;;;###autoload
(cl-defun trapt-apt-list (&key packages arglist (prompt t) remote)
  "Run apt list. This is a wrapper function for `trapt--execute'.

PACKAGES is space-separated string of packages to upgrade. If no PACKAGES are
passed, then the user will be prompted for a space-separated string containing
the list of packages to upgrade.

ARGLIST is a space-separated string of arguments to the apt command. If no
ARGLIST is passed, then the user will be prompted for a space-separated string
containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "list"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)))

(defun trapt-edit-sources ()
  "Opens `/etc/apt/sources.list' for editing as root user using tramp sudo."
  (interactive)
  (find-file "/sudo::/etc/apt/sources.list"))

(transient-define-prefix trapt--apt-upgrade-transient ()
  "Transient menu for apt upgrade commands."
  ["APT Upgrade Commands"
   ["Arguments"
    ("-y" "assume yes" "--assume-yes")
    ("-w" "with new packages" "--with-new-pkgs")]
   ["Run"
    ("c" "autoclean" trapt-apt-autoclean)
    ("f" "full upgrade" trapt-apt-full-upgrade)
    ("u" "update" trapt-apt-update)
    ("U" "upgrade" trapt-apt-upgrade)]
   ["Host"
    ("R" "remote host" "remote")]])

(transient-define-prefix trapt--apt-install-transient ()
  "Transient menu for apt install command."
  ["APT Install"
   ["Arguments"
    ("-n" "no recommends" "--no-install-recommends")
    ("-r" "reinstall" "--reinstall")
    ("-y" "assume yes" "--assume-yes")]
   ["Run"
    ("i" "install" trapt-apt-install)]
   ["Host"
    ("R" "remote host" "remote")]])

(transient-define-prefix trapt--apt-remove-transient ()
  "Transient menu for apt remove commands."
  ["APT Removal Commands"
   ["Arguments"
    ("-y" "assume yes" "--assume-yes")]
   ["Run"
    ("a" "autoremove" trapt-apt-autoremove)
    ("p" "purge" trapt-apt-purge)
    ("r" "remove" trapt-apt-remove)]
   ["Host"
    ("R" "remote host" "remote")]])

(transient-define-prefix trapt--apt-list-transient ()
  "Transient menu for apt list command."
  ["APT List"
   ["Arguments"
    ("-a" "all versions" "--all-versions")
    ("-i" "installed" "--installed")
    ("-u" "upgradable" "--upgradable")]
   ["Run"
    ("l" "list" trapt-apt-list)]
   ["Host"
    ("R" "remote host" "remote")]])

;;;###autoload (autoload 'trapt "A transient menu for APT." nil t)
(transient-define-prefix trapt ()
  "Transient menu for apt package manager."
  ["APT Package Manager"
   ["Edit Sources"
    ("e" "edit sources" trapt-edit-sources)]
   ["Sub-menus"
    ("i" "install packages" trapt--apt-install-transient)
    ("l" "list packages" trapt--apt-list-transient)
    ("r" "remove/purge packages" trapt--apt-remove-transient)
    ("u" "update/upgrade/autoclean" trapt--apt-upgrade-transient)]])

(provide 'trapt)

;;; trapt.el ends here
