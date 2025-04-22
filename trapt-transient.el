;;; trapt-transient.el --- Tranisent menus to control APT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20240420
;; Package-Requires: (transient)
;; Homepage: tbd
;; Keywords: APT transient


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

;; This package is part of trapt. It creates transient menus which can
;; control the advanced package tool (APT).

;;; Code:

(require 'transient)
(require 'trapt-utils)
(require 'trapt-list)

(defmacro trapt-transient--apt (function)
  "Executes FUNCTION passing arguments from 'transient-current-command'."
  (let ((arguments (gensym "trapt-")))
    `(let ((arguments
            (or (transient-args transient-current-command)
                "")))
       (,function nil arguments))))

(defun trapt-transient--apt-list ()
  "Wrapper function for 'trapt-apt-list'. Passes arguments from
  'transient-current-command' to 'trapt-apt-list'."
  (interactive)
  (trapt-transient--apt trapt-apt-list))

(defun trapt-transient--apt-purge ()
  "Wrapper function for 'trapt-apt-purge'. Passes arguments from
  'transient-current-command' to 'trapt-apt-purge'."
  (interactive)
  (trapt-transient--apt trapt-apt-purge))

(defun trapt-transient--apt-autoremove ()
  "Wrapper function for 'trapt-apt-autoremove'. Passes arguments from
  'transient-current-command' to 'trapt-apt-autoremove'."
  (interactive)
  (trapt-transient--apt trapt-apt-autoremove))

(defun trapt-transient--apt-remove ()
  "Wrapper function for 'trapt-apt-remove'. Passes arguments from
  'transient-current-command' to 'trapt-apt-remove'."
  (interactive)
  (trapt-transient--apt trapt-apt-remove))

(defun trapt-transient--apt-install ()
  "Wrapper function for 'trapt-apt-install'. Passes arguments from
  'transient-current-command' to 'trapt-apt-install'."
  (interactive)
  (trapt-transient--apt trapt-apt-install))

(defun trapt-transient--apt-upgrade ()
  "Wrapper function for 'trapt-apt-upgrade'. Passes arguments from
  'transient-current-command' to 'trapt-apt-upgrade'."
  (interactive)
  (trapt-transient--apt trapt-apt-upgrade))

(defun trapt-transient--apt-full-upgrade ()
  "Wrapper function for 'trapt-apt-full-upgrade'. Passes arguments from
  'transient-current-command' to 'trapt-apt-full-upgrade'."
  (interactive)
  (trapt-transient--apt trapt-apt-full-upgrade))

(transient-define-prefix trapt-transient--apt-upgrade-transient ()
  "Transient menu for apt list command"
  ["APT Package Manager"
   ["Arguments"
    ("-y" "assume yes" "--assume-yes")
    ("-w" "with new packages" "--with-new-pkgs")]
   ["Run"
    ("u" "upgrade" trapt-transient--apt-upgrade)
    ("f" "full upgrade" trapt-transient--apt-full-upgrade)]])

(transient-define-prefix trapt-transient--apt-install-transient ()
  "Transient menu for apt install command"
  ["APT Package Manager"
   ["Arguments"
    ("-n" "no recommends" "--no-install-recommends")
    ("-r" "reinstall" "--reinstall")
    ("-y" "assume yes" "--assume-yes")]
   ["Run"
    ("i" "install" trapt-transient--apt-install)]])

(transient-define-prefix trapt-transient--apt-remove-transient ()
  "Transient menu for apt remove commands"
  ["APT Package Manager"
   ["Arguments"
    ("-y" "assume yes" "--assume-yes")]
   ["Run"
    ("a" "autoremove" trapt-transient--apt-autoremove)
    ("p" "purge" trapt-transient--apt-purge)
    ("r" "remove" trapt-transient--apt-remove)]])

(transient-define-prefix trapt-transient--apt-list-transient ()
  "Transient menu for apt list command"
  ["APT Package Manager"
   ["Arguments"
    ("-i" "installed" "--installed")
    ("-u" "upgradable" "--upgradable")]
   ["Run"
    ("l" "list" trapt-transient--apt-list)]])

;;;###autoload (autoload 'trapt "Start a transient menu interface for APT package manager." nil t)
(transient-define-prefix trapt ()
  "Transient menu for apt package manager."
  ["APT Package Manager"
   ("c" "autoclean" trapt-apt-autoclean)
   ("i" "install packages" trapt-transient--apt-install-transient)
   ("l" "list packages" trapt-transient--apt-list-transient)
   ("r" "remove/purge packages" trapt-transient--apt-remove-transient)
   ("u" "update" trapt-apt-update)
   ("U" "upgrade" trapt-transient--apt-upgrade-transient)])

(provide 'trapt-transient)

;;; trapt-transient.el ends here.
