;;; trapt-utils.el --- Helper functions for trapt -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 2025-04-20
;; Package-Requires: (dependencies)
;; Homepage: tbd
;; Keywords: APT


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

;; This file contains helper functions for the trapt pacakge.

;;; Code:


(require 'tablist)

(defmacro trapt-utils--check-mode (mode &body)
  `(if (string= mode-name ,mode)
       ,&body
     (error (format "Function can only be run in %s mode." ,mode))))

(defun trapt-utils--list-or-string (value)
  "Returns VALUE if VALUE is a string. If VALUE is a list, returns a space-separated string using the items in list VALUE by calling 'trapt-utils--list-to-string'. If VALUE is nil, returns an empty string."
  (cond ((not value)
         "")
        ((stringp value)
         value)
        ((listp value)
         (trapt-utils--list-to-string value))))

(defun trapt-utils--build-command-string (operation &optional packages arguments)
  (trapt-utils--list-to-string (list "sudo"
                                     "apt"
                                     operation
                                     packages
                                     (trapt-utils--list-or-string arguments))))

(defun trapt-utils--run-command (command-string)
  "Run the COMMAND-STRING with 'async-shell-command'."
  (message (concat "Running: " command-string))
  (async-shell-command command-string))

(defun trapt-utils--list-to-string (lst)
  "Take a list of arguments LST and return them as a string separated by space."
  (cond ((not lst) nil)
        ((listp lst) (mapconcat #'identity lst " "))
        (t (error "Error: Must be a list."))))

(defun trapt-utils--shell-command-to-string (command-string)
  (let ((command-output (shell-command-to-string command-string)))
    (message (concat "Running: " command-string))
    command-output))

(defmacro trapt-utils--refresh-entries (promise)
  "Update the current buffer with the results of PROMISE."
  `(let ((buffer (current-buffer))
         (entries (aio-await ,promise)))
     (with-current-buffer buffer
       (setq tabulated-list-entries entries)
       (tabulated-list-print t))))

(provide 'trapt-utils)

;;; trapt-utils.el eds here.
