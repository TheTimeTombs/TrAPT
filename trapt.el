;;; trapt.el --- Control APT from Emacs -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250420
;; Package-Requires: (org transient)
;; Homepage: tbd
;; Keywords: APT transient org


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

;; Trapt is a package that allows users to be able to interact with th
;; advanced pacakge tool APT in Emacs using interactive functions,
;; transient menus, and Org mode. It also provides APT List mode
;; in which the ouput of 'apt list' is displayed in a tabulated
;; list buffer.

;;; Code:


(require 'trapt-utils)
(require 'trapt-list)
(require 'trapt-org)
(require 'trapt-transient)

(defgroup trapt nil "Customization options for Trapt")

(defvar trapt-apt-list-columns '("Name" "Version" "Architecture" "Status"))

(defmacro trapt--execute (operation &optional pkgs arglist)
  "Runs an APT command from an inferior shell.
OPERATATION can be any "
  (let ((packages (gensym "trapt-"))
        (arguments (gensym "trapt-"))
        (command (gensym "trapt-")))
    `(progn
       (when (get-buffer "*APT List*")
         (trapt-list--update-package-names))
       (let* ((,packages (or ,pkgs
                             (trapt-utils--list-to-string
                              trapt-list-marked-packages)
                             (read-string
                              (format "Enter packages to %s (space separarted -- enter for all): " ,operation))))
              (,arguments (or ,arglist
                              (read-string (format "Enter apt %s arguments (space separated -- enter for none): " ,operation))))
              (,command (trapt-utils--build-command-string
                         ,operation
                         ,packages
                         ,arguments)))
         (trapt--apt-operation ,operation ,command)))))

(defun trapt--apt-operation (operation command)
  (if (string= operation "list")
      ;; TODO run this asynchronously or speed up for large lists
      (trapt-utils--shell-command-to-string command)
    (trapt-utils--run-command command)))

;;;###autoload
(defun trapt-apt-list (&optional packages arglist)
  "Calls 'trapt-list--apt-list-to-tablist' to create a tablist buffer
containing the result of 'apt list' run from in an inferior shell.
Arguments can be passed to 'apt list' as the list ARGLIST or by
the transient menu 'trapt-transient--apt-list-transient'."
  (interactive)
  (let* ((apt-output (trapt--execute "list" nil arglist)))
    (trapt-list--apt-list-to-tablist "*APT List*" apt-output)))

;;;###autoload
(defun trapt-apt-install (&optional packages arglist)
  "Define me"
  (interactive)
  (trapt--execute "install" packages arglist))

;;;###autoload
(defun trapt-apt-remove (&optional packages arglist)
  "Define me"
  (interactive)
  (trapt--execute "remove" packages arglist))

;;;###autoload
(defun trapt-apt-upgrade (&optional packages arglist)
  "Run apt upgrade.

PACKAGES is a list or space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a list or space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "upgrade" ""))

;;;###autoload
(defun trapt-apt-purge (&optional packages arglist)
  "Run apt purge.

PACKAGES is a list or space-separated string of packages to upgrade.
If no PACKAGES are passed, then the user will be prompted for a
space-separated string containing the list of packages to upgrade.

ARGLIST is a list or space-separated string of arguments to the apt command.
If no ARGLIST is passed, then the user will be prompted for a
space-separated string containing the list of arguments to pass."
  (interactive)
  (trapt--execute "purge" ""))

;;;###autoload
(defun trapt-apt-autoremove ()
  "Run apt autoremove."
  (interactive)
  (trapt--execute "autoremove"))

;;;###autoload
(defun trapt-apt-full-upgrade (&optional packages arglist)
  "Run apt full-upgrade."
  (interactive)
  (trapt--execute "full-upgrade" packages arglist))

;;;###autoload
(defun trapt-apt-autoclean ()
  "Run apt autoclean."
  (interactive)
  ;; Pass empty strings to prevent prompts
  (trapt--execute "autoclean" "" ""))

;;;###autoload
(defun trapt-apt-update ()
  "Run apt update."
  (interactive)
  ;; Pass empty strings to prevent prompts
  (trapt--execute "update" "" ""))

(provide 'trapt)

;;; trapt.el ends here.
