;;; trapt-list.el --- Interact with APT List -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 2.0
;; Package-Requires: ((emacs "24.4") (easymenu) (tablist) (bui))
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

(defgroup trapt-list nil
  "TrAPT preferences for working with APT list."
  :group 'TrAPT
  :prefix "trapt-list-")

(defvar trapt-list--current-command nil
  "The command used to generate the current list.")

(defvar trapt-list-search-term nil
  "The command used to generate the current list.")

(defvar trapt-list--buffer-name "*APT List*"
  "The name of the buffer created for APT List Mode.")

(defvar trapt-list--mode-name "TrAPT List"
  "The name of `trapt-list-mode' buffer.")

(defvar trapt-list--num-installed nil
  "The number of installed APT packages.")

(defvar trapt-list--num-upgradable nil
  "The number of upgradable APT packages.")

(defvar trapt-list--num-residual nil
  "The number of APT packages with residual data.")

(defvar trapt-list--num-auto-intalled nil
  "The number of automatically installed APT packages.")

(defvar trapt-list--entries nil
  "A list of all the APT List entries for `tabulated-list-entries'.")

(easy-menu-define trapt-apt-list-list-mode-menu trapt-list-mode-map
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

(defun trapt-list--get-stats ()
  "Return a list of statistics from APT list."
  (thread-last
    (cl-loop for element in tabulated-list-entries
             when  (string-match "upgradable" (aref (cadr element) 4))
             count element into num-upgradable
             when (string-match "installed" (aref (cadr element) 4))
             count element into num-installed
             when (string-match "residual-config" (aref (cadr element) 4))
             count element into num-residual
             when (string-match "automatic" (aref (cadr element) 4))
             count element into num-auto
             finally
             return
             (if (not (string-match "--upgradable" trapt-list--current-command))
                 (cl-values `(trapt-list--num-installed . ,num-installed)
                            `(trapt-list--num-upgradable . ,num-upgradable)
                            `(trapt-list--num-residual . ,num-residual)
                            `(trapt-list--num-auto-installed . ,num-auto))
               ;; Only update upgradable stat if that list is called
               (cl-values`(trapt-list--num-upgradable . ,num-upgradable))))
    (trapt-utils--set-save-stats)))

(defun trapt-list--get-entries ()
  "Create an entry list for `bui-define-interface'."
  (cl-labels
      ((remove-unwanted-lines (apt-lines-list)
         "Remove unwanted messages from APT-LINES-LIST."
         (cl-remove-if (lambda (item)
                         (or (string-empty-p item)
                             (string-prefix-p "N:" item)
                             (string-prefix-p "WARNING:" item)
                             (string-prefix-p "Listing" item)
                             (string-prefix-p "Listing..." item)))
                       apt-lines-list))
       
       (lines-to-entries (apt-lines-list)
         "Convert each line from APT-LINES-LIST list to an entry"
         (cl-loop for line in apt-lines-list
                  for counter from 1
                  collect (if (= (length line) 4)
                              `((id . ,(make-symbol (nth 0 line)))
                                (name . ,(nth 0 line))
                                (source . ,(nth 1 line))
                                (version . ,(nth 2 line))
                                (architecture . ,(nth 3 line))
                                (status . "none"))
                            `((id . ,(make-symbol (nth 0 line)))
                              (name . ,(nth 0 line))
                              (source . ,(nth 1 line))
                              (version . ,(nth 2 line))
                              (architecture . ,(nth 3 line))
                              (status . ,(nth 4 line)))))))

    (thread-last
      (split-string
       (trapt-utils--shell-command-to-string trapt-list--current-command
                                             trapt-current-host)
       "\n")
      (remove-unwanted-lines)
      (mapcar #'(lambda (item) (split-string item  "[ /]")))
      (lines-to-entries))))

(defun trapt-list-set-search-term ()
  "Prompt the user for `trapt-list-search-term' and set it."
  (interactive)
  (thread-last
    (read-string "Search Term: ")
    (setf trapt-list-search-term)))

(bui-define-interface trapt-apt-list list
  :mode-name trapt-list--mode-name
  :buffer-name trapt-list--buffer-name
  :get-entries-function 'trapt-list--get-entries
  ;;:describe-function 'guix-store-item-list-describe
  :format '((name nil 20 t)
            (source nil 20 t)
            (version nil 20 t)
            (architecture nil 20 t)
            (status nil 20 t))
  :sort-key '(name)
  :marks '((install . ?I)))

(transient-define-prefix trapt-list--apt-list-transient ()
  "Transient menu for apt list command."
  ["Arguments"
   ("a" "all versions" "--all-versions")
   ("i" "installed" "--installed")
   ("u" "upgradable" "--upgradable")]
  ["APT List"
   ("l" "list" trapt-apt-list)]
  ["Search Term"
   ("s" "search term" trapt-list-set-search-term
    :transient t
    :description (lambda () (concat "Search Term: "
                                    trapt-list-search-term)))]
  ["Host"
   ("H" "host" trapt-set-host
    :transient t
    :description (lambda () (format "Host: %s" trapt-current-host)))])

;;;###autoload
(cl-defun trapt-apt-list (&key search-term arglist)
  "Run apt list. This is a wrapper function for `trapt--execute'.

SEARCH-TERM is a string to pass to `apt list' to search.

ARGLIST is a list of arguments to the apt command. If no ARGLIST is passed, then
the user will be prompted for a space-separated string containing the list of
arguments to pass."
  (interactive)
  (thread-last
    (trapt-utils--build-command-string "list"
                                       nil
                                       (or search-term
                                           trapt-list-search-term)
                                       (trapt--transient-args arglist))
    (setf trapt-list--current-command))
  (bui-get-display-entries 'apt-list 'list))

(provide 'trapt-list)

;;; trapt-list.el ends here
