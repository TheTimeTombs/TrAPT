;;; trapt-exec-find.el --- Identify and manage Emacs configuration dependencies -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250504
;; Package-Requires:
;; Homepage: https://github.com/tfree87/trapt
;; Keywords: trapt apt


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

;; TrAPT Exec Find defines a function for Emacs configuration files
;; `trapt-exec-find'. When a string with an executable program is passed to this
;; function, the executable name is stored. A report can be generated to
;; with `trapt-exec-find-report' to view all the programs. If the program is
;; installed on the host system, the path to the executable will be displayed.
;; Applications can additionally be managed using the APT package manager from
;; the report interface.

;;; Code:


(require 'trapt-core)
(require 'trapt-utils)

(defgroup TrAPT-Exec-Find nil
  "Customization options for TrAPT-Exec-Find."
  :group 'TrAPT)

(defvar trapt-exec-find--columns '("Name"
                                   "Path"
                                   "Version"
                                   "Package Manager"
                                   "Calling File")
  "A list of column names for `trapt-exec-find-mode'.")

(defcustom trapt-exec-find-default-sort-key '("Name" . nil)
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
                                                 (error "Default Sort Key must match a column name"))
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defvar trapt-exec-find-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'trapt-exec-find-goto-path)
    (define-key map "c" #'trapt-exec-find-goto-call)
    (define-key map "i" #'trapt-apt-install)
    (define-key map "I" #'trapt-apt-reinstall)
    (define-key map "R" #'trapt-apt-remove)
    (define-key map "P" #'trapt-apt-purge)
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
  (setf tabulated-list-entries ())
  (let*  ((counter 0)
          (entries
           (cl-loop for element in trapt-exec-find--list
                    do (setf counter (+ 1 counter))
                    collect `(,counter [,(car element)
                                        ,(trapt-exec-find--progpath (car element))
                                        ,(nth 2 element)
                                        ,(nth 3 element)
                                        ,(nth 1 element)]))))
    (setf tabulated-list-entries entries)))

(defun trapt-exec-find-goto-path ()
  "Opens the path for the executable at point."
  (interactive)
  (trapt-utils--check-mode "TrAPT Exec Find"
                           (find-file
                            (file-name-directory
                             (trapt-utils--get-tablist-item 1)))))

(defun trapt-exec-find-goto-call ()
  "Opens the file in which `trapt-exec-find' was called for the item at point."
  (interactive)
  (trapt-utils--check-mode "TrAPT Exec Find"
                           (find-file (trapt-utils--get-tablist-item 4))))

;;;###autoload
(cl-defun trapt-exec-find (command-string
                           &key
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

VERSION is an optional string that specifies the program version. Currently,
this is for reference purposes only.

PKG-MGR is an optional string containg the name of the package manager used to
manage this package. Currently, this if for reference purposes only."
  (let ((program (file-name-nondirectory (car (split-string command-string)))))
    (when (stringp program)
      (unless (member program trapt-exec-find--list)
        (push `(,program ,load-file-name ,version ,pkg-mgr) trapt-exec-find--list))))
  command-string)

;; Clear TrAPT--marked-packages when list buffer closed
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (string-equal (buffer-name)
                                trapt-exec-find--report-buffer-name)
              (setf trapt--marked-packages nil))))

;;;###autoload
(defun trapt-exec-find-report ()
  "Generate a report of all packages identified with `trapt-exec-find'."
  (interactive)
  (unless (member trapt-exec-find--report-buffer-name trapt--buffer-names)
    (push trapt-exec-find--report-buffer-name trapt--buffer-names))
  (with-current-buffer
      (get-buffer-create trapt-exec-find--report-buffer-name)
    (trapt-exec-find-mode)
    (setf tabulated-list-sort-key trapt-exec-find-default-sort-key)
    (setf tabulated-list-format [("Name" 15 t)
                                 ("Path" 30 t)
                                 ("Version" 15 t)
                                 ("Package Manager" 15 t)
                                 ("Calling File" 30 t)])
    (tabulated-list-init-header)
    (trapt-exec-find--create-tablist-entry-list)
    (revert-buffer))
  (switch-to-buffer "*TrAPT Exec Find*"))

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
