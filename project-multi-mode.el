;;; project-multi-mode.el --- Project.el integration with cmake and autoconf. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/projects-multi-mode
;; Keywords: project, cmake, build
;; Version: 1.2
;; Package-Requires: ((emacs "28"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Multiple backends for project.el (cmake, autotools)


;;; Code:
(require 'project)

(defgroup project-multi nil
  "Group for project-multi backend package."
  :group 'xref)

(defcustom project-multi-eagerly t
  "Configure the whole build information eagerly. This may add user
questions during the startup."
  :type 'boolean)

(defcustom project-multi-integrate-eglot t
  "Add eglot integration."
  :type 'boolean)

(defvar project-multi--backends-alist
  '((:type cmake
	   :program "cmake"
	   :root-hint "CMakeLists.txt"
	   :build-hint "CMakeCache.txt"
	   :project-regex "project[[:space:]]*([[:space:]]*\\([^[:space:]\n)]+\\)[^)]*)"
	   :compile-command ":program --build :build-dir"
	   :test-command "ctest --test-dir :build-dir"
	   )
    (:type automake
	   :program "make"
	   :root-hint "configure"
	   :build-hint "config.log"
	   :project-regex "PACKAGE_NAME='\\(.+\\)'"
	   :compile-command ":program -C :build-dir"
	   :test-command ":program -C :build-dir test"
	   )
    (:type meson
	   :program "meson"
	   :root-hint "meson.build"
	   :build-hint "meson-info"
	   :project-regex "project[[:space:]]*([[:space:]]*'\\([^']+\\)'[^)]+)"
	   :compile-command ":program compile -C :build-dir"
	   :test-command ":program test -C :build-dir"
	   )
    (:type cargo
	   :program "cargo"
	   :root-hint "Cargo.toml"
	   :build-hint "CACHEDIR.TAG"
	   :project-regex "name[[:space:]]=[[:space:]]\"\\([^\"]+\\)\""
	   :compile-command ":program build --target-dir :build-dir"
	   :test-command ":program test --target-dir :build-dir"
	   ))
  "Alist with backend information.
:compile-command may be a function that receives the
`project-multi--plist' and returns a string or a string with annotations
like plist keys to be substituted in the return string.")
(put 'project-multi--backends-alist 'risky-local-variable t)

(defvar project-multi--alist nil
  "Full list of project-multi roots.
The address is absolute for remote hosts.")
(put 'project-multi--alist 'risky-local-variable t)

(defvar-local project-multi--plist nil
  "Local access to the project plist.")

(defun project-multi--format-command (format-string project-plist)
  "Replace a formatted string FORMAT-STRING with keys values from PROJECT-PLIST.
Receives a FORMAT-STRING with annotations like PROJECT-PLIST keys and
returns a new string with all substitutions."
  (let ((pl project-plist)
        (fm format-string)
	newval)
    (while-let ((key (car pl)))
      (setq newval (format "%s" (cadr pl)))
      (setq fm (string-replace (symbol-name key)
			       (or (file-remote-p newval 'localname)
				   newval)
			       fm)
	    pl (cddr pl)))
    fm))

(defun project-multi--find-root (dir backend)
  "List DIR's dominant directories containing a `:root-hint' for BACKEND.
This function partially initializes the project's plist with
basic members.  Other functions latter will append extra information on
demand when the information requires user interaction.  AS NOW, the extra
information added latter will be `:compile-command' and `:build-dir'"
  (let ((root-hint (plist-get backend :root-hint))
	(root))
    (while-let ((path (and dir
			   (locate-dominating-file dir root-hint))))
      (setq root path)
      (setq dir (file-name-directory (directory-file-name path))))

    (when root
      (list :project-multi (plist-get backend :type)
	    :root (expand-file-name root)
	    :name (project-multi--get-project-name root backend)))))

(defun project-multi--get-backend (project-plist)
  "Get the backend from `project-multi--backends-alist' given a PROJECT-PLIST."
  (let ((project-type (plist-get project-plist :project-multi)))
    (seq-find (lambda (plist)
		(eq (plist-get plist :type) project-type))
	      project-multi--backends-alist)))

(defun project-multi--get-project-name (root backend)
  "Parse the :root-hint file from BACKEND in ROOT to get the project name.
Simply parses the :root-hint and search for the project name and returns
the capture group in :project-regex."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents-literally
       (expand-file-name (plist-get backend :root-hint) root))
      (when (re-search-forward (plist-get backend :project-regex) nil t)
	(match-string-no-properties 1)))))

(defun project-multi--get-subdirectories-with (root hint)
  "This function searches a file named HINT inside the first level sub-directories of ROOT.
The search is performed only in the first level directories and
non-recursively.  If the compile hint is in more than one subdirectory
this function returns the list with all the candidates."
  (when (file-exists-p root)
    (delq nil        ;; Get the list of directories in root with a :build-hint file
	  (mapcar
	   (lambda (dirlist)
	     (and (eq (file-attribute-type (cdr dirlist)) t) ;; it is a directory
		  (not (string-suffix-p ".." (car dirlist))) ;; but not parent
		  (file-exists-p (expand-file-name hint (car dirlist)))
		  (car dirlist)))
	   (directory-files-and-attributes root t nil t)))))

(defun project-multi--get-build-dir (plist)
  "Get a single build_dir subdir in current :root's PLIST.
When there is no valid, subdir, this returns nil.
If there is only one possible build_dir, this will return it immediately.
When there are multiple alternatives, this will ask to the user for
which one to use for this session."
  (let* ((backend (project-multi--get-backend plist))
	 (root (plist-get plist :root))
	 (hint (plist-get backend :build-hint))
	 (build-dir-list (or (project-multi--get-subdirectories-with root hint)
			     ;; search in all current subdirs else inside ./build if exists
			     (project-multi--get-subdirectories-with
			      (expand-file-name "build" root)
			      hint))))

    ;; If only one candidate, return it, else ask to the user.
    (if (cdr build-dir-list)
	(completing-read "Build directory: " build-dir-list nil t)
      (car build-dir-list))))

;; Utilities functions (a bit less low level) ========================

(defun project-multi--get-plist (dir)
  "Return the plist for DIR from `project-multi--alist'.
This function searches for a project associated with a root inside
project-multi--alist.  The search basically searches for a common prefix
in the path name because project-multi only supports top most roots."
  (seq-find (lambda (plist)
	      (string-prefix-p (plist-get plist :root) dir))
	    project-multi--alist))

(defun project-multi--create-plist (dir)
  "Return the project plist for the first valid root starting from DIR.
This iterates over `project-multi--backends-alist' until the function
`project-multi--find-root' finds a valid root.  Then it creates a plist
for the project that is inserted in the cache `project-multi--alist'.
This function returns the created plist."
  (when-let* ((default-directory dir)
	      (root-plist (let ((out nil)
				(in project-multi--backends-alist))
			    (while (and in
					(not (setq out (project-multi--find-root dir (pop in))))))
			    out)))

    ;; Find executable in the host
    (if-let* ((backend (project-multi--get-backend root-plist))
	      (executable (executable-find (plist-get backend :program) t)))
	(setq root-plist (plist-put root-plist :program executable))
      (user-error "No executable: %s found in %s machine"
		  program
		  (or (file-remote-p default-directory 'host)
		      "local")))

    ;; OK we found that there is some root hint in a parent directory of dir.
    ;; the call to project-multi--find-root returned a plist for this backend
    ;; We know that project.el will stop in this backend and won't search any
    ;; other, but it is useful to know if some other backend is valid for the same root
    ;; so, we execute the rest of the hooks in project-multi-project-backend and remember
    ;; the result in a sub-list inside the :others key.
    (let ((thisroot (plist-get root-plist :root))
	  (other-backends
	   (cdr (member 'project-multi-project-backend project-find-functions))))

      (plist-put root-plist :others
		 (delq nil (mapcar (lambda (fun)
				     (when-let* ((proj (funcall fun dir))
						 (otherroot (project-root proj))
						 ((file-equal-p otherroot thisroot)))
					 proj))
				   other-backends))))
    ;; Ok now we can decide initialize build information eagerly
    (when project-multi-eagerly
      (project-multi--init-build-dir root-plist))

    (car (push root-plist project-multi--alist))))

(defun project-multi--merge-plist (old new)
  "Left merge plist OLD over NEW.
Values already set in OLD are not changed."
  (while-let ((key (car new))
	      (value (cadr new)))
    (if (plist-member old key)
	;; When the key exists in both plists and they are also plists
	;; we need to go recursively throw the subkeys
	(when (and (plistp (plist-get old key))
		   (plistp value))
	  (project-multi--merge-plist (plist-get old key) value))
      ;; When the key is not in the old plist, then we just set it
      ;; completely
      (setq old (plist-put old key value)))
    (setq new (cddr new)))
  old)

(defun project-multi--add-dir-local-var (directory new-pair)
  "Add a directory-local variable NEW-PAIR for MODE in DIRECTORY, preserving existing ones."

  (if-let* ((existing-class (car (alist-get directory dir-locals-directory-cache nil nil 'string-equal))))
      (let* ((existing-vars (when existing-class
			      (alist-get existing-class dir-locals-class-alist)))
	     (newval (push new-pair (alist-get nil existing-vars)))
	     (newclass (intern (format "eglot-multi--%s" directory))))

	(unless existing-class
	  (dir-locals-set-class-variables newclass (cons nil newval))
	  (dir-locals-set-directory-class directory newclass)))

    (message "Could not set dir local: %s" new-pair)))

(defun project-multi--set-eglot (project-plist)
  "Set the eglot variables in root's PLIST when possible."
  (let* ((eglot-complete (project-multi--merge-plist ;; merge with new values
			  (bound-and-true-p eglot-workspace-configuration)
			  `(:clangd (:initializationOptions
				     (:compilationDatabasePath ,(plist-get project-plist :build-dir)))))))

    ;; set the dir local variables, they will apply automatically to
    ;; all buffers open in the future within the project root
    (project-multi--add-dir-local-var (plist-get project-plist :root)
				      `(eglot-workspace-configuration . (,eglot-complete)))

    ;; set the variable manually in all the already opened buffers
    ;; TODO: JAM check if the variable is not already set in the other buffers??
    ;; Probably override only the value instead of replacing the whole variable?
    (mapc (lambda (buffer)
	    (with-current-buffer buffer
	      (hack-dir-local-variables)))
	  (project-buffers project-plist)))

  (when-let* (((bound-and-true-p eglot--managed-mode))
	      (server (eglot-current-server)))
    (message "Signaling Eglot server")
    (eglot-signal-didChangeConfiguration server)))

(declare-function eglot-signal-didChangeConfiguration eglot)
(declare-function eglot-current-server eglot)

;; project integration ===============================================
(defun project-multi-project-backend (dir)
  "Return the project plist for DIR.
The search is performed in 3 steps in order to optimize an reduce
redundant expensive operations (specially with TRAMP).
1. Check buffer local variable `project-multi--plist'; which is set
locally the first time we call this function from a buffer.
2. Search for a root in `project-multi--alist' with a common prefix
with DIR.
3. Perform a hard search in the filesystem for project hint files."
  (if (local-variable-p 'project-multi--plist)
      project-multi--plist
    (setq-local project-multi--plist
		(or (project-multi--get-plist (expand-file-name dir))
		    (project-multi--create-plist (expand-file-name dir))))))

(cl-defmethod project-root ((project (head :project-multi)))
  "Root for PROJECT."
  (plist-get project :root))

(defun project-multi--init-build-dir (project-plist)
  "Return build directory for the current PROJECT.
The compile directory needs to be added lazily because the modeline
attempts to call `project-current' And this function attempts to call
`completing-read' when there are multiple build directories candidates
inside the root.  That results in an error."
  (let ((build-dir (project-multi--get-build-dir project-plist))
	(build-database))

    ;; This is set unconditionally, if nil set it to nil
    (setq project-plist (plist-put project-plist :build-dir build-dir))

    (when build-dir
      (setq build-database (expand-file-name "compile_commands.json" build-dir))
      (when  (file-exists-p build-database)
	(setq project-plist (plist-put project-plist :compile-database build-database))
	(when project-multi-integrate-eglot
	  (project-multi--set-eglot project-plist))))))

(defvar-local project-multi--current-info nil
  "Internal variable to track the accion/info taking place at the moment.
This will be set on action call in order to access from
project-multi--buffer-name-function because we cannot pass more
arguments to that function as it format is predefined.")

;; TODO: Improve this function to get a better format
(defun project-multi--buffer-name-function (name-of-mode)
  "Function to name the compilation buffers with more precise information."
  (let ((project (project-current t)))
    (concat "*"
	    (symbol-name project-multi--current-info)
	    " ["
	    (symbol-name (plist-get project :project-multi))
	    "] "
	    (project-name project)
	    "*")))

(defmacro project-multi-info-command (info)
  "Generate a cl-defmethod for a command
This performs substitution and initialization if needed."
  `(cl-defmethod project-extra-info ((project (head :project-multi))
				    (info (eql ,info)))
    "Return INFO compile command for current PROJECT."
    (unless (plist-member project :build-dir)
      (project-multi--init-build-dir project))

    (setq project-multi--current-info ,info)

    (when-let* (((not (plist-member project ,info)))
		(backend (project-multi--get-backend project))
		(command (plist-get backend ,info)))

      ;; We set this in the buffer local variable to support multiple
      ;; open projects.
      (setq project (plist-put project
			       ,info
			       (cond ((stringp command)
				      (project-multi--format-command command project))
				     ((functionp command)
				      (funcall command project))))))
    (plist-get project ,info)))

(project-multi-info-command :compile-command)
(project-multi-info-command :test-command)

(cl-defmethod project-name ((project (head :project-multi)))
  "Return all buffers in PROJECT."
  (plist-get project :name))

(cl-defmethod project-files ((project (head :project-multi)) &optional dirs)
  "Return all files in PROJECT
The compile projects doesn't provide file list information, so, this
function relies on the :other backends."
  (if-let* ((other-backends (plist-get project :others)))
      (catch 'found
	(while other-backends
	  (let* ((backend (car other-backends))
		 (result (project-files backend dirs)))
	    (when result (throw 'found result))
	    (setq other-backends (cdr other-backends)))))
    (error "Project %s doesn't have other backends" (project-name project))))


(cl-defmethod project-extra-info ((project (head :project-multi))
				  key)
  (or (plist-get project-extra-info key)
      (plist-get project key)))

;;;###autoload
(define-minor-mode project-multi-mode
  "Use Multiple backends for project.el."
  :global t
  (cond
   (project-multi-mode
    (add-hook 'project-find-functions #'project-multi-project-backend)
    (setq project-compilation-buffer-name-function #'project-multi--buffer-name-function))
   (t
    (remove-hook 'project-find-functions #'project-multi-project-backend)
    (setq project-compilation-buffer-name-function nil)
    )))

(provide 'project-multi-mode)
;;; project-multi-mode.el ends here
