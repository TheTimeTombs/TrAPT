;;; trapt.el --- Emacs interface to APT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250504
;; Package-Requires: (org transient)
;; Homepage: https://github.com/tfree87/trapt
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
;; in which the output of 'apt list' is displayed in a tabulated
;; list buffer.

;;; Code:


(require 'trapt-core)
(require 'trapt-list)
(require 'trapt-transient)
(require 'trapt-org)
(require 'trapt-exec-find)

(defgroup TrAPT nil "Customization options for TrAPT."
  :group 'Processes
  :prefix "trapt-")

(provide 'trapt)

;;; trapt.el ends here.
