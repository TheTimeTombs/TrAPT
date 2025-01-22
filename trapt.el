(require 'transient)

(defun apt--args-to-string (arglist)
  "Take a list of arguments ARGLIST and return them as a string separated by
space."
  (mapconcat #'identity arglist " "))

(defun apt-remove (package)
  "Remove PACKAGE using apt."
  (interactive "sPackage to remove: ")
  (async-shell-command (concat "sudo apt remove -y " package)))

(defun apt-upgrade ()
  "Upgrade package lists using APT."
  (interactive)
  (async-shell-command "sudo apt upgrade -y"))

(defun apt-full-upgrade ()
  "Full upgrade package lists using APT."
  (interactive)
  (async-shell-command "sudo apt full-upgrade -y"))

(defun apt-update ()
  "Update package lists using APT."
  (interactive)
  (async-shell-command "sudo apt update"))

(defun apt--install (package &optional args)
  "Install a package with APT asynchrously."
  (interactive "sPackage to install: ")
  (let ((arguments (or (transient-args transient-current-command) nil)))
    (async-shell-command (concat "sudo apt install -y "
                                 package
                                 (apt--args-to-string arguments)))))

(defun apt--list (&optional args)
  "List packages with APT."
  (interactive)
  (let ((arguments (or (transient-args transient-current-command) nil)))
    (async-shell-command (concat "sudo apt list "
                                 (apt--args-to-string arguments)))))

(defun apt-remove (package)
  "Install a package with "
  (interactive "sPackage to remove: ")
  (async-shell-command (concat "sudo apt remove -y "
                               package)))

(defun apt-purge (package)
  "Run an asynchronous process to purge PACKAGE with apt."
  (interactive "sPackage to purge: ")
  (async-shell-command (concat "sudo apt purge -y "
                               package)))

(defun apt-autoremove ()
  "Run an asynchronous process to autoremove packages with apt."
  (interactive)
  (async-shell-command "sudo apt autoremove -y"))

(transient-define-prefix apt--install-transient ()
  "Transient menu for apt install command"
  ["APT Package Manager"
   ["Arguments"
    ("-n" "no recommends" "--no-install-recommends")
    ("-r" "reinstall" "--reinstall")]
   ["Run"
    ("i" "Install" apt--install)]])

(transient-define-prefix apt--list-transient ()
  "Transient menu for apt list command"
  ["APT Package Manager"
   ["Arguments"
    ("-i" "installed" "--installed")
    ("-u" "upgradable" "--upgradable")]
   ["Run"
    ("l" "List" apt--list)]])

;;;###autoload (autoload 'apt "apt" nil t)
(transient-define-prefix apt ()
  "Transient menu for apt package manager."
  ["APT Package Manager"
   ["Install/Remove"
    ("a" "Autoremove" apt-autoremove)
    ("i" "Install" apt--install-transient)
    ("p" "Purge" apt-purge)
    ("r" "Remove" apt-remove)]
   ["Information"
    ("l" "List Packages" apt--list-transient)]
   ["Update"
    ("u" "Update" apt-update)
    ("U" "Upgrade" apt-upgrade)
    ("f" "Full Upgrade" apt-full-upgrade)]])

(provide 'freemacs-apt)
