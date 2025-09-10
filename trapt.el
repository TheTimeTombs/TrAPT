;;; trapt.el --- Emacs interface to APT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 2.0
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

;; Use transient menus to interact with the APT package manager.

;;; Code:

(require 'shelly)
(require 'transient)
(require 'tablist)
(require 'trapt-utils)
(require 'trapt-exec-find)
(require 'trapt-list)
(require 'trapt-org)


;;; customization options

(defgroup trapt nil "Customization options for TrAPT."
  :group 'Processes
  :prefix "trapt-")

(defcustom trapt-apt-sourcelist-file-path "/etc/apt/sources.list"
  "The path to the APT sources."
  :type '(string)
  :group 'trapt)

(defcustom trapt-shell "default"
  "The shell to run TrAPT commands."
  :type '(string)
  :options '("default" "eshell" "vterm")
  :group 'trapt)

(defcustom trapt-default-host "localhost"
  "The default host for TrAPT operations.")


;;; variables

(defvar trapt-current-host trapt-default-host
  "The current host to run TrAPT commands.")

(defvar trapt--package-list-buffers '()
  "List of TrAPT buffers with package lists.")

(defvar trapt--num-automatic 0
  "The number of automatically installed packages.")

(defvar trapt--num-installed 0
  "The number of packages which can be upgraded.")

(defvar trapt--num-manual 0
  "The number of manually installed packages.")

(defvar trapt-num-upgradable 0
  "The number of packages which can be upgraded.")

(defvar trapt-num-residual 0
  "The number of packages with residual data remaining.")


;;; general functions

(defun trapt--calculate-stats ()
  "Run `apt list' and collect statistics."
  (message "Loading TrAPT stats...")
  (with-temp-buffer
    (insert (shelly-command-to-string "apt list --installed 2> /dev/null"
                                      :host trapt-current-host))
    (mark-whole-buffer)
    (setf trapt--num-installed (count-matches "installed"))
    (setf trapt--num-upgradable (count-matches "upgradable"))
    (setf trapt--num-residual (count-matches "residual"))
    (setf trapt--num-automatic (count-matches "automatic"))
    (setf trapt--num-manual (- trapt--num-installed trapt--num-automatic))))

(defun trapt--apt-cache-updated ()
  "Return a string showing the last update to APT cache."
  (format "last updated: %s"
          (substring (shell-command-to-string "stat -c %y /var/cache/apt/")
                     0
                     16)))

(cl-defun trapt--execute (operation &key packages arglist (prompt t) host sudo)
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

If HOST is in the form username@hostname, then the APT command will be run
on that corresponding remote host.

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
           (trapt-list--current-command command host))
          ((string= operation "show")
           (trapt-utils--shell-command-to-string command))
          (t
           (trapt-utils--run-command command trapt-shell host)))))

(defun trapt--get-packages (packages)
  ""
  (message (buffer-name))
  (if packages
      packages
    (when (member (buffer-name) trapt--package-list-buffers)
      (thread-last
        (bui-list-get-marked)
        (mapcar (lambda (item) (symbol-name (car item))))
        (trapt-utils--list-to-string)))))

;;;###autoload
(cl-defun trapt-apt-install (&key packages arglist (prompt t))
  "Run apt install. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls."
  (interactive)
  (trapt--execute "install"
                  :packages (trapt--get-packages packages)
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-reinstall (&key packages arglist (prompt t))
  "Run apt reinstall. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls."
  (interactive)
  (trapt--execute "reinstall"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-remove (&key packages arglist (prompt t))
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls."
  (interactive)
  (trapt--execute "remove"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-upgrade (&key arglist prompt)
  "Run apt upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will be prompted for packages and arguments
if none are given."
  (interactive)
  (trapt--execute "upgrade"
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-purge (&key packages arglist (prompt t))
  "Run apt purge. This is a wrapper function for `trapt--execute'.

PACKAGES is a list of packages to upgrade. If no PACKAGES are passed, then the
user will be prompted for a space-separated string containing the list of
packages to upgrade.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is nil, then the user will not be prompted for packages and arguments
if none are given. This should be used for non-interactive calls."
  (interactive)
  (trapt--execute "purge"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-autoremove (&key arglist prompt remote)
  "Run apt autoremove.This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will be prompted for packages and arguments
if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt autoremove'."
  (interactive)
  (trapt--execute "autoremove"
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-full-upgrade (&key arglist prompt remote)
  "Run apt full-upgrade. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt full-upgrade'."
  (interactive)
  (trapt--execute "full-upgrade"
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-build-dep (&key arglist prompt remote)
  "Run apt build-dep. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt build-dep'."
  (interactive)
  (trapt--execute "build-dep"
                  :packages (trapt--get-marked-packages packages)
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-autoclean (&key arglist prompt remote)
  "Run apt autoclean.  This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt autoclean'."
  (interactive)
  (trapt--execute "autoclean"
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

;;;###autoload
(cl-defun trapt-apt-update (&key arglist prompt remote)
  "Run apt update. This is a wrapper function for `trapt--execute'.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass.

If PROMPT is non-nil, then the user will not be prompted for packages and
arguments if none are given.

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt update'."
  (interactive)
  (trapt--execute "update"
                  :arglist (trapt--transient-args arglist)
                  :prompt prompt
                  :host trapt-current-host
                  :sudo t))

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

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt show'."
  (interactive)
  (get-buffer-create "*APT Show*")
  (switch-to-buffer "*APT Show*")
  (erase-buffer)
  (insert
   (trapt--execute "show"
                   :packages (trapt--get-marked-packages packages)
                   :arglist (trapt--transient-args arglist)
                   :prompt prompt
                   :host trapt-current-host
                   :host nil)))

;;;###autoload
(cl-defun trapt-apt-moo (&key remote)
  "Run apt moo. This is a wrapper function for `trapt--execute'.

If REMOTE in non-nil, then the user will be prompted for a remote host from
`trapt-remotes' on which to run `apt moo'."
  (interactive)
  (trapt--execute "moo"
                  :packages nil
                  :arglist nil
                  :prompt nil
                  :host trapt-current-host
                  :sudo nil))

;;;###autoload
(cl-defun trapt-apt-edit-sources ()
  "Opens `/etc/apt/sources.list' for editing as root user using tramp."
  (interactive)
  (let* ((host (if (trapt--transient-remote remote)
                   (format "ssh:%s|" (trapt--get-host t))
                 ""))
         (path (format "/%ssudo::%s"
                       host
                       trapt-apt-sourcelist-file-path)))
    (find-file path)))


;;; helper functions for transient menus

(defun trapt--num-installed ()
  "Format installed packages number for display."
  (format "installed: %s" trapt--num-installed))

(defun trapt--num-manual ()
  "Format manually installed packages number for display."
  (format "manually installed: %s" trapt--num-manual))

(defun trapt--num-upgradable ()
  "Format upgradable packages number for display."
  (format "upgradable: %s" trapt--num-upgradable))

(defun trapt--num-residual ()
  "Return a string showing the number of upgradable packages."
  (format "residual configs: %s" trapt--num-residual))

(defun trapt--num-automatic ()
  "Format automatically isntalled packages number for display."
  (format "auto installed: %s" trapt--num-automatic))

(defun trapt-select-host ()
  "Prompt the user for a host and set `trapt-current-remote'."
  (interactive)
  (setf trapt-current-host (shelly-select-host))
  (trapt--calculate-stats))

(defun trapt--transient-args (arglist)
  "Remove the value `remote' from ARGLIST and return the shortened list."
  (if (bound-and-true-p transient-current-command)
      ;; Return transient args
      ;; or an empty string to prevent unwanted prompts for args
      (or (transient-args transient-current-command) "")
    arglist))


;;; transient menus and helper functions

(transient-define-prefix trapt--apt-upgrade-transient ()
  "Transient menu for apt upgrade commands."
  ["Host"
   ("H" "host" trapt-select-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))]
  ["APT Package Info"
   (:info #'trapt--num-upgradable)]
  ["Arguments"
   ("s" "simulate" "--simulate")
   ("y" "assume yes" "--assume-yes")
   ("w" "with new packages" "--with-new-pkgs")]
  ["APT Upgrade/Autoclean"
   ("c" "autoclean" trapt-apt-autoclean)
   ("f" "full upgrade" trapt-apt-full-upgrade)
   ("u" "update" trapt-apt-update)
   ("U" "upgrade" trapt-apt-upgrade)])

(transient-define-prefix trapt--apt-install-transient ()
  "Transient menu for apt install command."
  ["Host"
   ("H" "host" trapt-select-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))]
  ["APT Package Info"
   (:info #'trapt--num-installed)
   (:info #'trapt--num-automatic)
   (:info #'trapt--num-manual)]
  ["Arguments"
   ("d" "download only" "--download-only")
   ("n" "no recommends" "--no-install-recommends")
   ("r" "reinstall" "--reinstall")
   ("s" "simulate" "--simulate")
   ("y" "assume yes" "--assume-yes")]
  ["APT Install"
   ("i" "install" trapt-apt-install)])

(transient-define-prefix trapt--apt-remove-transient ()
  "Transient menu for apt remove commands."
  ["Host"
   ("H" "host" trapt-select-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))]
  ["APT Package Info"
   (:info #'trapt--num-installed)
   (:info #'trapt--num-automatic)
   (:info #'trapt--num-manual)
   (:info #'trapt--num-residual)]
  ["Arguments"
   ("s" "simulate" "--simulate")
   ("y" "assume yes" "--assume-yes")]
  ["APT Removal Commands"
   ("a" "autoremove" trapt-apt-autoremove)
   ("p" "purge" trapt-apt-purge)
   ("r" "remove" trapt-apt-remove)])

(transient-define-prefix trapt--apt-other-transient ()
  "Transient menu for apt package manager."
  ["APT Package Manager"
   ("e" "edit sources" trapt-apt-edit-sources)
   ("m" "moo" trapt-apt-moo)
   ("s" "show" trapt-apt-show)]
  ["Host"
   ("H" "host" trapt-select-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))])

(transient-define-prefix trapt--transient ()
  "Transient menu for apt package manager."
  ["Host"
   ("H" "host" trapt-select-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))]
  ["APT Package Info"
   (:info #'trapt--num-installed)
   (:info #'trapt--num-automatic)
   (:info #'trapt--num-manual)
   (:info #'trapt--num-upgradable)
   (:info #'trapt--num-residual)]
  ["APT Package Manager"
   ("i" "install packages" trapt--apt-install-transient)
   ("l" "list packages" trapt-list--apt-list-transient)
   ("o" "other commands" trapt--apt-other-transient)
   ("r" "remove/purge packages" trapt--apt-remove-transient)
   ("u" "update/upgrade/autoclean" trapt--apt-upgrade-transient)])

;;;###autoload
(defun trapt ()
  "Run `trapt--calculate-state' and then `trapt--transient'.
This function is the man entry point for TrAPT."
  (interactive)
  (trapt--calculate-stats)
  (trapt--transient))

(provide 'trapt)

;;; trapt.el ends here
