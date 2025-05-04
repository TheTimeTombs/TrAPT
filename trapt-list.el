;;; trapt-list.el --- Interact with APT List -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 20250420
;; Package-Requires: (tablist)
;; Homepage: https://github.com/tfree87/trapt
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

;; This package is part of of trapt. This package provides features to
;; pipe the output of APT list to a tabulated list buffer. Packages can
;; be marked and then APT commands can be executed on the selection.

;;; Code:


(require 'async)
(require 'tablist)

(defgroup TrAPT-List nil
  "Trapt preferences for working with APT list"
  :group 'TrAPT
  :prefix "trapt-list")

(defvar trapt-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'trapt-apt-install)
    (define-key map "I" #'trapt-apt-reinstall)
    (define-key map "R" #'trapt-apt-remove)
    (define-key map "P" #'trapt-apt-purge)
    map)
  "Keymap for `trapt-list-mode'.")

(defvar trapt-list--columns '("Name" "Version" "Architecture" "Status")
  "A list of column name for `trapt-list-mode'.")

(defvar trapt-list--marked-packages nil
  "The list of packges marked in the current APT List Buffer.")

(defvar trapt-list--buffer-name "*APT List*"
  "The name of the buffer created for APT List Mode")

(defvar trapt-list--mode-name "TrAPT List"
  "The name of `trapt-list-mode' buffer.")

(defcustom trapt-list-default-sort-key '("Name" . nil)
  "Sort key for for sorting results returned from apt list.

This should be a cons cell (NAME . FLIP) where NAME is a string matching one of
the column names from `trapt-list--columns' and FLIP is a boolean to specify
the sort order."
  :group 'trapt-list
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (member
                                            (widget-value widget)
                                            trapt-list--columns)
                                     (widget-put widget
                                                 (error "Default Sort Key must match a column name"))
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

;; Clear trapt-list--marked-packages when list buffer closed
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (string-equal (buffer-name) trapt-list--buffer-name)
              (setf trapt-list--marked-packages nil))))

(defun trapt-list--update-package-names ()
  "Updates variable'trapt-lsit-marked-packages' with the currently marked
packages from the *APT List* buffer."
  (when (get-buffer trapt-list--buffer-name)
    (progn (switch-to-buffer trapt-list--buffer-name)
           (when (caar (tablist-get-marked-items))
             (let ((result-list
                    (cl-loop for item in (tablist-get-marked-items)
                             collect (aref (cdr item) 0))))
               (setf trapt-list--marked-packages (reverse result-list)))))))

(defun trapt-list--create-tablist-entry-list (apt-list-output)
  "Take a list from APT-LIST-ITEMS and add them to `tabulated-list-entries'."
  (setf tabulated-list-entries ())
  (let ((apt-list-lines (trapt-list--create-tablist-entries apt-list-output))
        (entries '())
        (counter 1))
    (dolist (element apt-list-lines)
      (add-to-list 'entries `(,counter [,@element]))
      (setf counter (+ 1 counter)))
    (setf tabulated-list-entries entries)))

(defun trapt-list--process-lines (apt-list-output)
  "Splits the output of APT-LIST-OUTPUT.
 by space and \ delimiters and returns
  the list APT-LIST-ENTRIES."
  (let ((apt-list-entries (mapcar (lambda (line) (split-string line "[ /]"))
                                  (trapt-list--apt-list-split-lines apt-list-output))))
    apt-list-entries))

(defun trapt-list--apt-list-split-lines (apt-list-output)
  "Split APT-LIST-OUTPUT lines while removing unwanted lines."
  (cl-remove-if (lambda (item)
                  (or (string-empty-p item)
                      (string-prefix-p "WARNING:" item)
                      (string-prefix-p "Listing..." item)))
                (split-string apt-list-output "\n")))

(defun trapt-list--apt-list-to-tablist (buffer-name apt-list-output)
  "Create a tablist buffer with name BUFFER-NAME.
The tablist buffer is populated with entries from APT-LIST-OUTPUT."
  (with-current-buffer (get-buffer-create buffer-name)
    (trapt-list-mode)
    (setf tabulated-list-sort-key trapt-list-default-sort-key)
    (setf tabulated-list-format [("Name" 30 t)
                                 ("Source" 25 t)
                                 ("Version" 15 t)
                                 ("Architecture" 8 t)
                                 ("Status" 50 t (:right-align t))])
    (tabulated-list-init-header)
    (trapt-list--create-tablist-entry-list apt-list-output)
    (revert-buffer))
  (switch-to-buffer buffer-name))

;;;###autoload
(define-derived-mode trapt-list-mode tabulated-list-mode trapt-list--mode-name
  "Major mode for interacting with a list of packages from APT."
  :keymap trapt-list-mode-map
  (setf tabulated-list-padding 2)
  (tablist-minor-mode))

(provide 'trapt-list)

;;; trapt-list.el ends here.
