;;; trapt-exec-find.el --- Identify .emacs.d  -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 2.0
;; Package-Requires: ((emacs "24.3") (bui))
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


(require 'bui)
(require 'trapt-utils)

(defgroup trapt-exec-find nil
  "Customization options for TrAPT-Exec-Find."
  :group 'TrAPT)

(defvar trapt-exec-find-list-mode-map
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

(easy-menu-define trapt-exec-find-list-mode-menu trapt-exec-find-list-mode-map
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
  (trapt-exec-find--propertize (or (executable-find program) "not found")))

(defun trapt-exec-find--get-entries ()
  "Pass `trapt-exec-find-list' and add them to `tabulated-list-entries'.
Additionally, determine the execuable paths for each executable and pass them to
`tabulated-list-entries'."
  (cl-loop for element in trapt-exec-find--list
           collect `((id . ,(make-symbol (nth 0 element)))
                     (package . ,(nth 0 element))
                     (executable . ,(nth 1 element))
                     (path . ,(trapt-exec-find--progpath (nth 1 element)))
                     (version . ,(trapt-exec-find--propertize (nth 2 element)))
                     (package-manager . ,(nth 3 element))
                     (calling-path . ,(trapt-exec-find--propertize (nth 4 element))))))

(bui-define-interface trapt-exec-find list
  :mode-name "TrAPT Exec Find"
  :buffer-name "*TrAPT Exec Find*"
  :get-entries-function 'trapt-exec-find--get-entries
  ;;:describe-function 'guix-store-item-list-describe
  :format '((package nil 15 t)
            (executable nil 15 t)
            (path nil 30 t)
            (version nil 15 t)
            (package-manager nil 15 t)
            (calling-path nil 30 t))
  :sort-key '(package)
  :marks '((install . ?I)))

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
                           (version "not specified")
                           (pkg-mgr "not specified"))
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
         (pkg-name (or pkg-name program))
         (version (trapt-exec-find--propertize version))
         (pkg-mgr (trapt-exec-find--propertize pkg-mgr))
         (proglist (cl-loop for elt in trapt-exec-find--list
                            collect (car elt))))
    (when (stringp program)
      (unless (member program proglist)
        (push `(,pkg-name ,program ,version ,pkg-mgr ,(or load-file-name
                                                          "not specified"))
              trapt-exec-find--list))))
  command-string)

(defun trapt-exec-find--propertize (string)
  "Take STRING and return a propertized string based on string text."
  (cond ((string= string "not specified")
         (propertize string 'font-lock-face 'font-lock-comment-face))
        ((string= string "not found")
         (propertize string 'font-lock-face 'font-lock-warning-face))
        (t
         (propertize string 'font-lock-face 'font-lock-string-face))))

;;;###autoload
(defun trapt-exec-find-report ()
  "Generate a report of all packages identified with `trapt-exec-find'."
  (interactive)
  (bui-get-display-entries 'trapt-exec-find 'list))

(provide 'trapt-exec-find)

;;; trapt-exec-find.el ends here
