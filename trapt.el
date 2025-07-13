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

(defcustom trapt-stats-file "~/.emacs.d/trapt-stats.eld"
  "A string with the path to the TrAPT statisics file."
  :type '(string)
  :group 'trapt)

(defcustom trapt-apt-sourcelist-file-path "/etc/apt/sources.list"
  "A list of remote ssh connections for TrAPT.

`trapt-remotes' should be a quoted list in which each element of the list is in
the form username@server."
  :type '(string)
  :group 'trapt)

(defcustom trapt-shell "default"
  "The shell to run TrAPT commands."
  :type '(string)
  :options '("default" "eshell" "vterm")
  :group 'trapt)

(defvar trapt--tablist-buffers '()
  "A list of tablist buffer names with package tablists for TrAPT.")

(defun trapt--num-installed ()
  "Return a string showing the number of installed packages."
  (when (and (bound-and-true-p trapt-list--num-installed)
             (> trapt-list--num-installed 0))
    (format "installed packages: %s" trapt-list--num-installed)))

(defun trapt--num-upgradable ()
  "Return a string showing the number of ugradable packages."
  (when (and (bound-and-true-p trapt-list--num-upgradable)
             (> trapt-list--num-upgradable 0))
    (format "upgradable packages: %s" trapt-list--num-upgradable)))

(defun trapt--num-residual ()
  "Return a string showing the number of ugradable packages."
  (when (and (bound-and-true-p trapt-list--num-residual)
             (> trapt-list--num-residual 0))
    (format "residual configs: %s" trapt-list--num-residual)))

(defun trapt--load-stats ()
  "Load data from `trapt-stats-file', it if exists."
  (when (file-exists-p trapt-stats-file)
    (cl-loop for elt in (trapt-utils--read-file trapt-stats-file)
             do (set `,(car elt) (cdr elt)))))

(defun trapt--num-automatic ()
  "Return a string showing the number of installed packages."
  (when (and (bound-and-true-p trapt-list--num-auto-installed)
             (> trapt-list--num-auto-installed 0))
    (format "installed automatically: %s" trapt-list--num-auto-installed)))

(defun trapt--transient-remote (remote)
  "Wrapper function for `trapt--get-server'.

If the current function was called by a transient menu, call `trapt--get-server'
while passing a non-nil value if `remote' is in the list of tranisent aruments
as determined by `tranisent-args'

Otherwise call `trapt--get-server' passing the value REMOTE."
  (if (bound-and-true-p transient-current-command)
      (let ((arguments (transient-args transient-current-command)))
        ;; Check to see if user called `remote' argument
        (trapt--get-server (member "remote" arguments)))
    (trapt--get-server remote)))

(defun trapt--transient-args-clean (arglist)
  "Remove the value `remote' from ARGLIST and return the shortened list."
  (if (bound-and-true-p transient-current-command)
      (let* ((arguments (transient-args transient-current-command))
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

(defun trapt--apt-cache-updated ()
  "Return a string showng the last update to APT cache."
  (format "Updated: %s"
          (substring (shell-command-to-string "stat -c %y /var/cache/apt/")
                     0
                     16)))

(cl-defun trapt--execute (operation &key packages arglist (prompt t) server sudo)
  "Run an APT command from an inferior shell.

OPERATION must be a string and can be any command understood by the APT package
manager. Including full-upgrade, install, reinstall, purge, remove, update, and
upgrade.

PACKAGES is list or space-separated string of packages to upgrade. If no
PACKAGES are passed, then the user will be prompted for a space-separated string
containing the list of packages to upgrade.

ARGLIST is a list or space-separated string of arguments to the apt command. If
no ARGLIST is passed, then the user will be prompted for a space-separated
string containing the list of arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given.

If SERVER is in the form username@servername, then the APT command will be run
on that corresponding remote server.

If SUDO is non-nil, then the command will be run with sudo."
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
                   sudo
                   packages
                   arguments)))
    (cond ((string= operation "list")
           (trapt-list--create-tablist command server)
           (when (or (not packages) (string= packages ""))
             (trapt-list--update-stats)))
          ((string= operation "show")
           (trapt-utils--shell-command-to-string command))
          (t
           (trapt-utils--run-command command trapt-shell server)))))

;;;###autoload
(cl-defun trapt-apt-install (&key packages arglist (prompt t) remote)
  "Run apt install. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt install'."
  (interactive)
  (trapt--execute "install"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-reinstall (&key packages arglist (prompt t) remote)
  "Run apt reinstall. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt reinstall'."
  (interactive)
  (trapt--execute "reinstall"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-remove (&key packages arglist (prompt t) remote)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (trapt--execute "remove"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-upgrade (&key arglist prompt remote)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will be prompted for packages and arguments
if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt upgrade'."
  (interactive)
  (trapt--execute "upgrade"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-purge (&key packages arglist (prompt t) remote)
  "Run apt purge. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt purge'."
  (interactive)
  (trapt--execute "purge"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-autoremove (&key arglist prompt remote)
  "Run apt autoremove.This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will be prompted for packages and arguments
if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt autoremove'."
  (interactive)
  (trapt--execute "autoremove"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-full-upgrade (&key arglist prompt remote)
  "Run apt full-upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt full-upgrade'."
  (interactive)
  (trapt--execute "full-upgrade"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-build-dep (&key arglist prompt remote)
  "Run apt build-dep. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt build-dep'."
  (interactive)
  (trapt--execute "build-dep"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-autoclean (&key arglist prompt remote)
  "Run apt autoclean.  This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt autoclean'."
  (interactive)
  (trapt--execute "autoclean"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-update (&key arglist prompt remote)
  "Run apt update. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt update'."
  (interactive)
  (trapt--execute "update"
                  :arglist (trapt--transient-args-clean arglist)
                  :prompt prompt
                  :server (trapt--transient-remote remote)
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-list (&key packages arglist (prompt t) remote)
  "Run apt list. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt list'."
  (interactive)
  (let* ((arguments (trapt--transient-args-clean arglist)))
    (trapt--execute "list"
                    :packages (trapt--get-marked-packages packages)
                    :arglist arguments
                    :prompt prompt
                    :server (trapt--transient-remote remote)
                    :sudo nil)))

;;;###autoload
(cl-defun trapt-apt-show (&key packages arglist (prompt t) remote)
  "Run apt list. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt show'."
  (interactive)
  (get-buffer-create "*APT Show*")
  (switch-to-buffer "*APT Show*")
  (erase-buffer)
  (insert
   (trapt--execute "show"
                   :packages (trapt--get-marked-packages packages)
                   :arglist (trapt--transient-args-clean arglist)
                   :prompt prompt
                   :server (trapt--transient-remote remote)
                   :server nil)))

;;;###autoload
(cl-defun trapt-apt-moo (&key remote)
  "Run apt moo. This is a wrapper function for `trapt--execute'.

If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt moo'."
  (interactive)
  (trapt--execute "moo"
                  :packages nil
                  :arglist nil
                  :prompt nil
                  :server (trapt--transient-remote remote)
                  :sudo nil))

;;;###autoload
(cl-defun trapt-apt-edit-sources (&key remote)
  "Opens `/etc/apt/sources.list' for editing as root user using tramp sudo.

  If REMOTE in non-nil, then the user will be prompted for a remote server from
`trapt-remotes' on which to run `apt remove'."
  (interactive)
  (let* ((server (if (trapt--transient-remote remote)
                     (format "ssh:%s|" (trapt--get-server t))
                   ""))
         (path (format "/%ssudo::%s"
                       server
                       trapt-apt-sourcelist-file-path)))
    (find-file path)))

(transient-define-prefix trapt--apt-upgrade-transient ()
  "Transient menu for apt upgrade commands."
  ["Info"
   (:info #'trapt--apt-cache-updated)
   (:info #'trapt--num-upgradable
          :if trapt--num-upgradable)]
  ["Arguments"
   ("-s" "simulate" "--simulate")
   ("-y" "assume yes" "--assume-yes")
   ("-w" "with new packages" "--with-new-pkgs")]
  ["APT Upgrade/Autoclean"
   ("c" "autoclean" trapt-apt-autoclean)
   ("f" "full upgrade" trapt-apt-full-upgrade)
   ("u" "update" trapt-apt-update)
   ("U" "upgrade" trapt-apt-upgrade)]
  ["Host"
   ("R" "remote host" "remote")])

(transient-define-prefix trapt--apt-install-transient ()
  "Transient menu for apt install command."
  ["Localhost Info"
   (:info #'trapt--num-installed
          :if trapt--num-installed)]
  ["Arguments"
   ("-d" "download only" "--download-only")
   ("-n" "no recommends" "--no-install-recommends")
   ("-r" "reinstall" "--reinstall")
   ("-s" "simulate" "--simulate")
   ("-y" "assume yes" "--assume-yes")]
  ["APT Install"
   ("i" "install" trapt-apt-install)]
  ["Host"
   ("R" "remote host" "remote")])

(transient-define-prefix trapt--apt-remove-transient ()
  "Transient menu for apt remove commands."
  ["Arguments"
   ("-s" "simlulate" "--simulate")
   ("-y" "assume yes" "--assume-yes")]
  ["APT Removal Commands"
   ("a" "autoremove" trapt-apt-autoremove)
   ("p" "purge" trapt-apt-purge)
   ("r" "remove" trapt-apt-remove)]
  ["Host"
   ("R" "remote host" "remote")])

(transient-define-prefix trapt--apt-other-transient ()
  "Transient menu for apt package manager."
  ["APT Package Manager"
   ("e" "edit sources" trapt-apt-edit-sources)
   ("m" "moo" trapt-apt-moo)
   ("s" "show" trapt-apt-show)]
  ["Host"
   ("R" "remote host" "remote")])

(transient-define-prefix trapt--apt-list-transient ()
  "Transient menu for apt list command."
  ["Arguments"
   ("-a" "all versions" "--all-versions")
   ("-i" "installed" "--installed")
   ("-u" "upgradable" "--upgradable")]
  ["APT List"
   ("l" "list" trapt-apt-list)]
  ["Host"
   ("R" "remote host" "remote")])

;;;###autoload (autoload 'trapt "trapt.el" "A transient menu for APT." t)
(transient-define-prefix trapt ()
  "Transient menu for apt package manager."
  ["Localhost Info"
   (:info #'trapt--num-installed
          :if trapt--num-installed)
   (:info #'trapt--num-automatic
          :if trapt--num-automatic)
   (:info #'trapt--num-upgradable
          :if trapt--num-upgradable)
   (:info #'trapt--num-residual
          :if trapt--num-residual)]
  ["APT Package Manager"
   ("i" "install packages" trapt--apt-install-transient)
   ("l" "list packages" trapt--apt-list-transient)
   ("o" "other commands" trapt--apt-other-transient)
   ("r" "remove/purge packages" trapt--apt-remove-transient)
   ("u" "update/upgrade/autoclean" trapt--apt-upgrade-transient)])

;; Load saved statistics after package load
(eval-after-load "trapt.el" (trapt--load-stats))

(provide 'trapt)

;;; trapt.el ends here
