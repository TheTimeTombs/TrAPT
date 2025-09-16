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


;;; constants

(defconst trapt-exec-find--mode-name "TrAPT Exec Find"
  "The name of the mode for `trapt-exec-find-mode'.")

(defconst trapt-exec-find--buffer-name "*TrAPT Exec Find*"
  "The name of the buffer for `trapt-exec-find-report'.")


;;; variables

(defvar trapt-exec-find--list nil
  "A list of executable programs for `trapt-exec-find-report'.
Each element of the list will be if the form program, version, and package
manager.")


;;; base functions

(defun trapt-exec-find--progpath (program)
  "Return the program path for PROGRAM or return `not found'."
  (trapt-exec-find--propertize (or (executable-find program) "not found")))


;;; define entries

(defun trapt-exec-find--exec->entry (exec)
  "Return for `trapt-exec-find-list' buffer entry for EXEC."
  (let ((item (assoc (format "%s" exec) trapt-exec-find--list)))
    `((id . ,(make-symbol (nth 0 item)))
      (package . ,(nth 0 item))
      (executable . ,(nth 1 item))
      (path . ,(trapt-exec-find--progpath (nth 1 item)))
      (version . ,(trapt-exec-find--propertize (nth 2 item)))
      (package-manager . ,(nth 3 item))
      (calling-path . ,(trapt-exec-find--propertize (nth 4 item))))))

(defun trapt-exec-find--get-execs (&optional search-type &rest search-values)
  "Return a list of `trapt-exec-find' entry types.

If SEARCH-TYPE is the symbol `all', then all of the possible executable entries
will be returned.

If the first element of SEARCH-TYPE is `id', then the list SEARCH-VALUES will be
used to return the list of executables to display."
  (or search-type (setf search-type 'all))
  (cl-case search-type
    (all (trapt-exec-find--exec-names))
    (id search-values)
    (t (error "Unknown search type: %S" search-type))))

(defun trapt-exec-find--exec-names ()
  "Return a list of all executable names from `trapt-exec-find--list'."
  (cl-loop for item in trapt-exec-find--list
           collect (car item)))

(defun trapt-exec-find--get-entries (&rest execs )
  "Return `trapt-exec-find' entries for EXECS."
  (mapcar #'trapt-exec-find--exec->entry
          (apply #'trapt-exec-find--get-execs execs)))

(bui-define-entry-type trapt-exec-find
  :get-entries-function #'trapt-exec-find--get-entries)


;;; info interface

(bui-define-interface trapt-exec-find info
  :format '((package format (format))
            (executable format (format))
            (path format (format))
            (version format (format))
            (package-manager format (format))
            (calling-path format (format))))


;;; list interface

(defun trapt-exec-find--describe (&rest execs)
  "Display 'info' buffer for EXECS."
  (bui-get-display-entries 'trapt-exec-find 'info (cons 'id execs)))

(bui-define-interface trapt-exec-find list
  :mode-name "TrAPT Exec Find"
  :buffer-name trapt-exec-find--buffer-name
  :describe-function #'trapt-exec-find--describe
  :format '((package nil 15 t)
            (executable nil 15 t)
            (path nil 30 t)
            (version nil 15 t)
            (package-manager nil 15 t)
            (calling-path nil 30 t))
  :sort-key '(package)
  :marks '((install . ?I)))

(let ((map trapt-exec-find-list-mode-map))
  (define-key map "p" #'trapt-exec-find-goto-path)
  (define-key map "c" #'trapt-exec-find-goto-call)
  (define-key map "x" #'trapt))

;; This must come after `bui-define-interface'
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

(defun trapt-exec-find-goto-path ()
  "Opens the path for the executable at point."
  (interactive)
  (if (string= (buffer-name) trapt-exec-find--buffer-name)
      (find-file
       (file-name-directory
        (cdr (assoc 'path (bui-list-current-entry)))))
    (error (format "This function can only be used in %s buffers."
                   trapt-exec-find--buffer-name))))

(defun trapt-exec-find-goto-call ()
  "Opens the file in which `trapt-exec-find' was called for the item at point."
  (interactive)
  (if (string= (buffer-name) trapt-exec-find--buffer-name)
      (find-file (cdr (assoc 'calling-path (bui-list-current-entry))))
    (error (format "This function can only be used in %s buffers."
                   trapt-exec-find--buffer-name))))

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
