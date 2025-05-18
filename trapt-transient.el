;;; trapt-transient.el --- Tranisent menus to control APT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20240508
;; Package-Requires: (transient)
;; Homepage: https://github.com/tfree87/trapt
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

;; This package is part of TrAPT. It creates transient menus which can
;; control the advanced package tool (APT).

;;; Code:


(require 'transient)
(require 'trapt-core)
(require 'trapt-list)

(defmacro trapt-transient--apt (function)
  "Execute FUNCTION passing arguments from `transient-current-command'."
  `(let* ((arguments (transient-args transient-current-command))
          ;; Check to see if user called `remote' argument
          (remote (member "remote" arguments))
          ;; Remove `remote' arugment if it is present
          (arglist (cl-remove-if #'(lambda (elt) (string= "remote" elt))
                                 arguments)))
     (if arglist
         (,function :arglist arglist :remote remote)
       ;; Send empty string as argument list
       (,function :arglist "" :remote remote))))

(transient-define-suffix trapt-transient--apt-list ()
  "Transient suffix for `trapt-apt-list'."
  (interactive)
  (trapt-transient--apt trapt-apt-list))

(transient-define-suffix trapt-transient--apt-purge ()
  "Transient suffix for `trapt-apt-purge'."
  (interactive)
  (trapt-transient--apt trapt-apt-purge))

(transient-define-suffix trapt-transient--apt-autoremove ()
  "Transient suffix for `trapt-apt-autoremove'."
  (interactive)
  (trapt-transient--apt trapt-apt-autoremove))

(transient-define-suffix trapt-transient--apt-remove ()
  "Transient suffix for `trapt-apt-remove'."
  (interactive)
  (trapt-transient--apt trapt-apt-remove))

(transient-define-suffix trapt-transient--apt-install ()
  "Transient suffix for `trapt-apt-install'."
  (interactive)
  (trapt-transient--apt trapt-apt-install))

(transient-define-suffix trapt-transient--apt-upgrade ()
  "Transient suffix for `trapt-apt-upgrade'."
  (interactive)
  (trapt-transient--apt trapt-apt-upgrade))

(transient-define-suffix trapt-transient--apt-full-upgrade ()
  "Transient suffix for `trapt-apt-full-upgrade'."
  (interactive)
  (trapt-transient--apt trapt-apt-full-upgrade))

(transient-define-suffix trapt-transient--apt-update ()
  "Transient suffix for `trapt-apt-update'."
  (interactive)
  (trapt-transient--apt trapt-apt-update))

(transient-define-prefix trapt-transient--apt-upgrade-transient ()
  "Transient menu for apt upgrade commands."
  ["APT Upgrade Commands"
   ["Arguments"
    ("-y" "assume yes" "--assume-yes")
    ("-w" "with new packages" "--with-new-pkgs")]
   ["Run"
    ("c" "autoclean" trapt-apt-autoclean)
    ("f" "full upgrade" trapt-transient--apt-full-upgrade)
    ("u" "update" trapt-transient--apt-update)
    ("U" "upgrade" trapt-transient--apt-upgrade)]
   ["Host"
    ("R" "remote host" "remote")]])

(transient-define-prefix trapt-transient--apt-install-transient ()
  "Transient menu for apt install command."
  ["APT Install"
   ["Arguments"
    ("-n" "no recommends" "--no-install-recommends")
    ("-r" "reinstall" "--reinstall")
    ("-y" "assume yes" "--assume-yes")]
   ["Run"
    ("i" "install" trapt-transient--apt-install)]
   ["Host"
    ("R" "remote host" "remote")]])

(transient-define-prefix trapt-transient--apt-remove-transient ()
  "Transient menu for apt remove commands."
  ["APT Removal Commands"
   ["Arguments"
    ("-y" "assume yes" "--assume-yes")]
   ["Run"
    ("a" "autoremove" trapt-transient--apt-autoremove)
    ("p" "purge" trapt-transient--apt-purge)
    ("r" "remove" trapt-transient--apt-remove)]
   ["Host"
    ("R" "remote host" "remote")]])

(transient-define-prefix trapt-transient--apt-list-transient ()
  "Transient menu for apt list command."
  ["APT List"
   ["Arguments"
    ("-a" "all versions" "--all-versions")
    ("-i" "installed" "--installed")
    ("-u" "upgradable" "--upgradable")]
   ["Run"
    ("l" "list" trapt-transient--apt-list)]
   ["Host"
    ("R" "remote host" "remote")]])

;;;###autoload (autoload 'trapt "A transient menu for APT." nil t)
(transient-define-prefix trapt ()
  "Transient menu for apt package manager."
  ["APT Package Manager"
   ["Sub-menus"
    ("i" "install packages" trapt-transient--apt-install-transient)
    ("l" "list packages" trapt-transient--apt-list-transient)
    ("r" "remove/purge packages" trapt-transient--apt-remove-transient)
    ("u" "update/upgrade/autoclean" trapt-transient--apt-upgrade-transient)]])

(provide 'trapt-transient)

;;; trapt-transient.el ends here.
