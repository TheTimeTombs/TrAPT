;;; trapt-utils.el --- Helper functions for TrAPT -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 1.2
;; Package-Requires: ((emacs "24.3") (tablist))
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


(defvar trapt-utils--apt-options '("-o \"apt::color=no\"")
  "A list of options to always pass to APT -o.
These options in these arguments are essential to ensure
proper parsing of the results that are returned from APT.")

(defmacro trapt-utils--check-mode (mode body)
  "BODY is a Lisp form that will execute if the current mode is MODE.
If not, an error will be thrown."
  `(if (string= mode-name ,mode)
       ,body
     (error (format "Error: Function must be called from %s mode." ,mode))))

(defmacro trapt-utils--run-ssh (host body)
  "Run BODY with tramp on HOST using ssh.

HOST should be in a format that can be passed to tramp (i.e.
                                                        username@hostname)."
  `(let* ((default-directory (expand-file-name
                              (format "/ssh:%s:~/" ,host))))
     (with-connection-local-variables ,body)))

(defun trapt-utils--list-or-string (value)
  "Return VALUE if VALUE is a string.
If VALUE is a list, return a space-separated string."
  (cond ((not value) "")
        ((stringp value) value)
        ((listp value) (trapt-utils--list-to-string value))))

(defun trapt-utils--build-command-string (operation
                                          &optional
                                          sudo
                                          packages
                                          arguments)
  "Concatenates the elements of an APT command string.

OPERATION is a string containing a command for the APT package tool.

If SUDO is non-nil then the command will be called with `sudo'.

PACKAGES is list or space-separated string of packages to upgrade.

ARGUMENTS is a list or space-separated string of arguments to the apt command.
If no ARGUMENTS is passed, then the user will be prompted for a space-separated
string containing the list of arguments to pass."
  (let ((package-string (trapt-utils--list-or-string packages))
        (arg-string (trapt-utils--list-or-string arguments)))
    (thread-last
      (when arg-string
        (concat " " arg-string))
      (concat (when package-string
                (concat " " package-string)))
      (concat " " operation)
      (concat (if sudo
                  "sudo apt"
                "apt")))))

(defun trapt-utils--list-to-string (lst)
  "Take a list of arguments LST and return them as a string separated by space."
  (cond ((not lst) nil)
        ((listp lst) (mapconcat #'identity lst " "))
        (t (error "Error: Must be a list!"))))

(provide 'trapt-utils)

;;; trapt-utils.el ends here
