;;; project-multi-mode.el --- Project.el integration with cmake and autoconf. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/projects-multi-mode
;; Keywords: project, cmake, build
;; Version: 1.1
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

(defvar project-multi--backends-alist
  '((:type cmake
	   :program "cmake"
	   :root-hint "CMakeLists.txt"
	   :build-hint "CMakeCache.txt"
	   :project-regex "project[[:blank:]]*([[:blank:]]*\\([[:alnum:]]+\\).+)"
	   :compile-command ":program --build ."
	   )
    (:type automake
	   :program "make"
	   :root-hint "configure"
	   :build-hint "config.log"
	   :project-regex "PACKAGE_NAME='\\(.+\\)'"
	   :compile-command ":program"
	   ))
  "CMake alist with backend information.
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
        (fm format-string))
    (while-let ((key (car pl))
		(value (cadr pl)))
      (setq fm (string-replace (symbol-name key) value fm)
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
      (setq root (concat (file-remote-p dir) root))

      (list :project-multi (plist-get backend :type)
	    :root root
	    :name (project-multi--get-project-name root backend)
	    :backend backend))))

(defun project-multi--get-project-name (root backend)
  "Parse the cmake file FILENAME to get the project name.
Simply parses the CMakeLists.txt and search for the project name parsing
the line project(name)."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents-literally
       (expand-file-name (plist-get backend :root-hint) root))
      (when (re-search-forward (plist-get backend :project-regex) nil t)
	(match-string-no-properties 1)))))

(defun project-multi--get-build-dir (plist)
  "Get a single build_dir subdir in current DIR.
When there is no valid, subdir, this returns nil.
If there is only one possible build_dir, this will return it immediately.
When there are multiple alternatives, this will ask to the user for
which one to use for this session."
  (let* ((backend (plist-get plist :backend))
	 (build-hint (plist-get backend :build-hint))
	 (build-dir-list
	  (delq nil        ;; Get the list of directories in root with a :build-hint file
		(mapcar
		 (lambda (dirlist)
		   (and (eq (file-attribute-type (cdr dirlist)) t)
			(not (string-suffix-p ".." (car dirlist)))
			(file-exists-p (expand-file-name build-hint (car dirlist)))
			(car dirlist)))
		 (directory-files-and-attributes (plist-get plist :root) t nil t 1)))))
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
    (car (push root-plist project-multi--alist))))

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
    (setq-local project-multi--plist (or (project-multi--get-plist dir)
					 (project-multi--create-plist dir)))))

(cl-defmethod project-root ((project (head :project-multi)))
  "Root for PROJECT."
  (plist-get project :root))

(cl-defmethod project-build-dir ((project (head :project-multi)))
  "Return build directory of the current PROJECT.

This needs to be added lazily because the modeline attempts to call
`project-current' And this function attempts to call `completing-read'
when there are multiple build directories candidates inside the root.
That results in an error."
  (unless (plist-member project :build-dir)
    ;; if project-build-dir var is set, then return nil ad project will reply on that.
    (setq project (plist-put project
			     :build-dir (unless project-build-dir
					  (project-multi--get-build-dir project)))))
  (plist-get project :build-dir))

(cl-defmethod project-compile-command ((project (head :project-multi)))
  "Return build command for current PROJECT."
  (unless (plist-member project :compile-command)
    (let* ((backend (plist-get project :backend))
	   (program (plist-get backend :program))
	   (executable (executable-find program t)))
      (unless executable
	(user-error "No %s executable found in %s machine"
		    program
		    (or (file-remote-p default-directory 'host)
			"local")))
      (setq project (plist-put project :program executable))
      (setq project (plist-put project
			       :compile-command
			       (let ((command (plist-get backend :compile-command)))
				 (cond ((stringp command)
					(project-multi--format-command command project))
				       ((functionp command)
					(funcall command project))))))))
  (plist-get project :compile-command))

(cl-defmethod project-name ((project (head :project-multi)))
  "Return all buffers in PROJECT."
  (plist-get project :name))

(cl-defmethod project-buffers ((project (head :project-multi)))
  "Return the list of all live buffers that belong to PROJECT."
  (mapcar (lambda (buff)
	    (cond
	     ((eq (buffer-local-value 'project-multi--plist buff) project) buff)
	     ((local-variable-p 'project-multi--plist buff) nil)
	     (t (with-current-buffer buff
		  (when (eq (project-multi-project-backend default-directory) project)
		    (current-buffer))))))
	  (buffer-list)))

;;;###autoload
(define-minor-mode project-multi-mode
  "Use Multiple backends for project.el."
  :global t
  (cond
   (project-multi-mode
    (add-hook 'project-find-functions #'project-multi-project-backend))
   (t
    (remove-hook 'project-find-functions #'project-multi-project-backend))))

(provide 'project-multi-mode)
;;; project-multi-mode.el ends here
