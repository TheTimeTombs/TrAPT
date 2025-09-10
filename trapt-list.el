;;; trapt-list.el --- Interact with APT List -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 2.0
;; Package-Requires: ((emacs "24.4") (easymenu) (bui))
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

;; This package is part of TrAPT. This package provides features to
;; pipe the output of APT list to a tabulated list buffer. Packages can
;; be marked and then APT commands can be executed on the selection.

;;; Code:


(require 'bui)
(require 'easymenu)
(require 'trapt-utils)
(require 'transient)


;;; constants

(defconst trapt-list--buffer-name "*APT List*"
  "The name of the buffer created for APT List Mode.")

(defconst trapt-list--mode-name "TrAPT List"
  "The name of `trapt-list-mode' buffer.")


;;; variables

(defvar trapt-list--current-command nil
  "The command used to generate the current list.")

(defvar trapt-list-search-term nil
  "The command used to generate the current list.")

(defvar trapt-list--entries nil
  "A list of all the APT List entries for `tabulated-list-entries'.")

(defvar trapt-list--names nil
  "A list of all the APT List entries for `tabulated-list-entries'.")


;; Create apt entry type

(defun trapt-list--package->entry (package)
  "Return entry for `PACKAGE' from ."
  (let ((item (assoc (format "%s" package) trapt-list--entries)))
    (if (= (length item) 4)
        `((id . ,(make-symbol (nth 0 item)))
          (package . ,(nth 0 item))
          (source . ,(nth 1 item))
          (version . ,(nth 2 item))
          (architecture . ,(nth 3 item))
          ;; The following will be used in `info' interfaces
          (status . "none")
          (version . "unknown")
          (priority . "unknown")
          (essential . "unknown")
          (section . "unknown")
          (maintainer . "unknown")
          (installed-size . "unknown")
          (provides . "unknown")
          (depends . "unknown")
          (download-size . "unknown")
          (apt-manual-installed . "unknown")
          (apt-sources . "unknown")
          (description . "unknown"))
      `((id . ,(make-symbol (nth 0 item)))
        (package . ,(nth 0 item))
        (source . ,(nth 1 item))
        (version . ,(nth 2 item))
        (architecture . ,(nth 3 item))
        (status . ,(nth 4 item))
        ;; The following will be used in `info' interfaces
        (status . "none")
        (version . "unknown")
        (priority . "unknown")
        (essential . "unknown")
        (section . "unknown")
        (maintainer . "unknown")
        (installed-size . "unknown")
        (provides . "unknown")
        (depends . "unknown")
        (download-size . "unknown")
        (apt-manual-installed . "unknown")
        (apt-sources . "unknown")
        (description . "unknown")))))

(defun trapt-list--package-names ()
  "Returns a list of all package names from `trapt-list--entries'."
  (cl-loop for item in trapt-list--entries
           collect (car item)))

(defun trapt-list--get-packages (&optional search-type &rest search-values)
  
  (or search-type (setf search-type 'all))
  (cl-case search-type
    (all (trapt-list--package-names))
    (id search-values)
    (t (error "Unknown search type: %S" search-type))))

(defun trapt-list--get-entries (&rest args)
  (mapcar #'trapt-list--package->entry
          (apply #'trapt-list--get-packages args)))

(defun trapt-list--generate (command)
  "Create an entry list for `bui-define-interface'."
  (cl-labels
      ((remove-unwanted-lines (apt-lines-list)
         "Remove unwanted items from APT-LINES-LIST."
         (cl-remove-if (lambda (item)
                         (or (string-empty-p item)
                             (string-prefix-p "N:" item)
                             (string-prefix-p "WARNING:" item)
                             (string-prefix-p "Listing" item)
                             (string-prefix-p "Listing..." item)))
                       apt-lines-list)))

    (thread-last
      (split-string
       (trapt-utils--shell-command-to-string command
                                             trapt-current-host)
       "\n")
      (remove-unwanted-lines)
      (mapcar (lambda (item) (split-string item "[ /]")))
      (setf trapt-list--entries))))

(bui-define-entry-type trapt-apt
  :get-entries-function #'trapt-list--get-entries)


;;; info interface

(bui-define-interface trapt-apt info
  :format '((package format (format))
            (source format (format))
            (version format (format))
            (architecture format (format))
            (status format (format))))

(defvar trapt-apt-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'trapt-org-export-all)
    (define-key map "m" #'trapt-org-export-marked)
    (define-key map "x" #'trapt)
    map)
  "Keymap for `trapt-apt-list-info-mode'.")

(easy-menu-define trapt-apt-info-mode-menu trapt-apt-info-mode-map
  "Menu when `trapt-list-mode' is active."
  `("TrAPT List"
    ["Install selected packages" trapt-apt-install
     :help "Install the selected packages with APT."]
    ["Purge selected packages" trapt-apt-purge
     :help "Purge selected packages with APT."]
    ["Reinstall selected packages" trapt-apt-reinstall
     :help "Reinstall selected packages with APT."]
    ["Reinstall selected packages" trapt-apt-remove
     :help "Remove selected packages with APT."]))


;; list interface

(defun trapt-apt-list--describe (&rest packages)
"Display entries for PACKAGES "
(bui-get-display-entries 'trapt-apt 'info (cons 'id packages)))

(bui-define-interface trapt-apt list
  :mode-name trapt-list--mode-name
  :buffer-name trapt-list--buffer-name
  :describe-function #'trapt-apt-list--describe
  :format '((package nil 20 t)
            (source nil 20 t)
            (version nil 20 t)
            (architecture nil 20 t)
            (status nil 20 t))
  :sort-key '(package)
  :marks '((install . ?I)))

(defvar trapt-apt-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'trapt-org-export-all)
    (define-key map "m" #'trapt-org-export-marked)
    (define-key map "x" #'trapt)
    map)
  "Keymap for `trapt-apt-list-mode'.")

(easy-menu-define trapt-apt-list-mode-menu trapt-apt-list-mode-map
"Menu when `trapt-list-mode' is active."
`("TrAPT List"
  ["Install selected packages" trapt-apt-install
   :help "Install the selected packages with APT."]
  ["Purge selected packages" trapt-apt-purge
   :help "Purge selected packages with APT."]
  ["Reinstall selected packages" trapt-apt-reinstall
   :help "Reinstall selected packages with APT."]
  ["Reinstall selected packages" trapt-apt-remove
   :help "Remove selected packages with APT."]))


;;; transient menu and helper functions

(defun trapt-list-set-search-term ()
  "Prompt the user for `trapt-list-search-term' and set it."
  (interactive)
  (thread-last
    (read-string "Search Term: ")
    (setf trapt-list-search-term)))

(transient-define-prefix trapt-list--apt-list-transient ()
  "Transient menu for apt list command."
  :value '("--installed")
  ["Host"
   ("H" "host" trapt-select-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))]
  ["Search Term"
   ("s" "search term" trapt-list-set-search-term
    :transient t
    :description (lambda () (concat "Search Term: "
                                    trapt-list-search-term)))]
  ["Arguments"
   ("a" "all versions" "--all-versions")
   ("i" "installed" "--installed")
   ("u" "upgradable" "--upgradable")]
  ["APT List"
   ("l" "list" trapt-apt-list)])



;;;###autoload
(cl-defun trapt-apt-list (&key search-term arglist)
  "Run apt list. This is a wrapper function for `trapt--execute'.

SEARCH-TERM is a string to pass to `apt list' to search.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass."
  (interactive)
  (thread-last
    (trapt-utils--build-command-string "list -o \"apt::color=no\""
                                       nil
                                       (or search-term
                                           trapt-list-search-term)
                                       (trapt--transient-args arglist))
    (trapt-list--generate))
  (bui-get-display-entries 'trapt-apt 'list))

(eval-after-load "trapt-list"
  '(add-to-list 'trapt--package-list-buffers trapt-list--buffer-name))

(provide 'trapt-list)

;;; trapt-list.el ends here
