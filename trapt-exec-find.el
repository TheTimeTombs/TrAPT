;;; trapt-exec-find.el --- Identify .emacs.d  -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250522
;; Package-Requires: ((emacs "24.3"))
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

;; TrAPT Exec Find defines the function `trapt-exec-find' for Emacs
;; configuration files. When a string with an executable program is passed to
;; this function, the executable name is stored. A report can be generated to
;; with `trapt-exec-find-report' to view all the programs. If the program is
;; installed on the host system, the path to the executable will be displayed.
;; Applications can additionally be managed using the APT package manager from
;; the report interface.

;;; Code:


(require 'trapt-utils)

(defgroup trapt-exec-find nil
  "Customization options for TrAPT-Exec-Find."
  :group 'TrAPT)

(defvar trapt-exec-find--columns '("Package Name"
                                   "Executable Name"
                                   "Path"
                                   "Version"
                                   "Package Manager"
                                   "Calling File")
  "A list of column names for `trapt-exec-find-mode'.")

(defcustom trapt-exec-find-default-sort-key '("Package Name" . nil)
  "Sort key for results returned from `trapt-exec-find-report'.

 This should be a cons cell (NAME . FLIP) where NAME is a string matching one of
the column names from `trapt-exec-find--columns' and FLIP is a boolean to
specify the sort order."
  :group 'trapt-exec-find
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (member
                                            (widget-value widget)
                                            trapt-exec-find--columns)
                                     (widget-put widget
                                                 (error "Default sort key must\
 match a column name"))
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defvar trapt-exec-find-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'trapt-exec-find-goto-path)
    (define-key map "c" #'trapt-exec-find-goto-call)
    (when (fboundp 'trapt)
      (define-key map "x" #'trapt))
    map)
  "Keymap for `trapt-exec-find-mode'.")

(defvar trapt-exec-find--list nil
  "A list of executable programs for `trapt-exec-find-report'.
Each element of the list will be if the form program, version, and package
manager.")

(defvar trapt-exec-find--mode-name "TrAPT Exec Find"
  "The name of the mode for `trapt-exec-find-mode'.")

(defvar trapt-exec-find--report-buffer-name "*TrAPT Exec Find*"
  "The name of the buffer for `trapt-exec-find-report'.")

(easy-menu-define trapt-exec-find-mode-menu trapt-exec-find-mode-map
  "Menu when `trapt-exec-find-mode' is active."
  `("TrAPT Exec Find"
    ["Install selected packages" trapt-apt-install
     :help "Install the selected packages with APT."]
    ["Purge selected packages" trapt-apt-purge
     :help "Purge selected packages with APT."]
    ["Reinstall selected packages" trapt-apt-reinstall
     :help "Reinstall selected packages with APT."]
    ["Reinstall selected packages" trapt-apt-remove
     :help "Remove selected packages with APT."]
    ["Go to Executable Path" 'trapt-exec-find-goto-path
     :help "Open the path location for the executable file at point."]
    ["Go to trapt-exec-find Call" 'trapt-exec-find-goto-call
     :help "Go to the Lisp file where 'trapt-exec-find' was called for the item\
 at point."]))

(defun trapt-exec-find--progpath (program)
  "Return the program path for PROGRAM or return `not found'."
  (let ((path (executable-find program)))
    (if path
        (propertize path 'font-lock-face 'font-lock-string-face)
      (propertize "not found" 'font-lock-face 'font-lock-warning-face))))

(defun trapt-exec-find--create-tablist-entry-list ()
  "Pass `trapt-exec-find-list' and add them to `tabulated-list-entries'.
Additionally, determine the execuable paths for each executable and pass them to
`tabulated-list-entries'."
  (let*  ((entries
           (cl-loop for element in trapt-exec-find--list
                    for counter from 1
                    collect `(,counter [,(car element)
                                        ,(nth 1 element)
                                        ,(trapt-exec-find--progpath
                                          (nth 1 element))
                                        ,(nth 2 element)
                                        ,(nth 3 element)
                                        ,(nth 4 element)]))))
    (setf tabulated-list-entries entries)))

(defun trapt-exec-find-goto-path ()
  "Opens the path for the executable at point."
  (interactive)
  (trapt-utils--check-mode "TrAPT Exec Find"
                           (find-file
                            (file-name-directory
                             (trapt-utils--get-tablist-item 2)))))

(defun trapt-exec-find-goto-call ()
  "Opens the file in which `trapt-exec-find' was called for the item at point."
  (interactive)
  (trapt-utils--check-mode "TrAPT Exec Find"
                           (find-file (trapt-utils--get-tablist-item 5))))

;;;###autoload
(cl-defun trapt-exec-find (command-string
                           &key
                           pkg-name
                           (version (propertize
                                     "not specified"
                                     'font-lock-face
                                     'font-lock-comment-face))
                           (pkg-mgr (propertize
                                     "not specified"
                                     'font-lock-face
                                     'font-lock-comment-face)))
  "Extract an executable name from a COMMAND-STRING.
The value is stored it in`trap-exec--find-list'. The original COMMAND-STRING
will be returned.

PKG-NAME is the name of the package if to install from the package manager if
that name differs from the first element of COMMAND-STRING.

VERSION is an optional string that specifies the program version. Currently,
this is for reference purposes only.

PKG-MGR is an optional string containg the name of the package manager used to
manage this package. Currently, this if for reference purposes only."
  (let* ((program (file-name-nondirectory (car (split-string command-string))))
         (pkg-name (or pkg-name program)))
    (when (stringp program)
      (unless (member program (cl-loop for elt in trapt-exec-find--list
                                       collect (car elt)))
        (push `(,pkg-name ,program ,version ,pkg-mgr ,load-file-name)
              trapt-exec-find--list))))
  command-string)

;;;###autoload
(defun trapt-exec-find-report ()
  "Generate a report of all packages identified with `trapt-exec-find'."
  (interactive)
  (with-current-buffer
      (get-buffer-create trapt-exec-find--report-buffer-name)
    (trapt-exec-find-mode)
    (when (boundp 'trapt--tablist-buffers)
      (if trapt--tablist-buffers
          (add-to-list 'trapt--tablist-buffers
                       trapt-exec-find--report-buffer-name)
        (push trapt-exec-find--report-buffer-name trapt--tablist-buffers)))
    (setf tabulated-list-sort-key trapt-exec-find-default-sort-key)
    (setf tabulated-list-format [("Package Name" 15 t)
                                 ("Executable Name" 15 t)
                                 ("Path" 30 t)
                                 ("Version" 15 t)
                                 ("Package Manager" 15 t)
                                 ("Calling File" 30 t)])
    (tabulated-list-init-header)
    (trapt-exec-find--create-tablist-entry-list)
    (revert-buffer))
  (switch-to-buffer trapt-exec-find--report-buffer-name))

;;;###autoload
(define-derived-mode trapt-exec-find-mode
  tabulated-list-mode
  trapt-exec-find--mode-name
  "Major mode for interacting with a list of packages from APT."
  :keymap trapt-exec-find-mode-map
  (setf tabulated-list-padding 2)
  (tablist-minor-mode))

(provide 'trapt-exec-find)

;;; trapt-exec-find.el ends here
