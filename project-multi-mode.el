;;; gtags-mode.el --- GNU Global integration with xref, project and imenu. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/gtags-mode
;; Keywords: xref, project, imenu, gtags, global
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

(defcustom project-multi-cmake-executable "cmake"
  "CMake executable."
  :type 'string
  :local t)

(defvar project-multi--alist nil
  "Full list of project-multi roots.
The address is absolute for remote hosts.")
(put 'project-multi--alist 'risky-local-variable t)

(defvar-local project-multi--cmake (executable-find project-multi-cmake-executable))

(defvar-local project-multi--plist nil
  "Project Global root for this buffer.")

;; Connection functions
(defun project-multi--set-connection-locals ()
  "Set connection local variables when possible and needed."
  (when-let* ((remote (file-remote-p default-directory))
	      ((not (local-variable-p 'project-multi--cmake)))
	      (criteria (connection-local-criteria-for-default-directory))
	      (symvars (intern (format "project-multi--%s-vars" remote)))
	      (enable-connection-local-variables t))
    (unless (alist-get symvars connection-local-profile-alist)
      (with-connection-local-variables  ;; because *-executable can be set as connection local
       (let ((cmake (if (local-variable-p 'project-multi-cmake-executable)
			 project-multi-cmake-executable
		       (file-name-nondirectory project-multi-cmake-executable))))
	 (connection-local-set-profile-variables
	  symvars
	  `((project-multi--cmake . ,(executable-find cmake t))))
	 (connection-local-set-profiles criteria symvars))))
    (hack-connection-local-variables-apply criteria)))

(defun project-multi--get-tree (dir)
  "Get a list of DIR's dominant directories containing a CMakeLists.txt."
  (let ((out))
    (while-let ((path (and dir
			   (locate-dominating-file dir "CMakeLists.txt"))))
      (push path out)
      (setq dir (file-name-directory (directory-file-name path))))
    out))

(defun project-multi--get-project-name (filename)
  "Parse the cmake file FILENAME to get the project name.
Simply parses the CMakeLists.txt and search for the project name parsing
the line project(name)."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (let ((case-fold-search nil))
      (re-search-forward "project[[:blank:]]*([[:blank:]]*\\([[:alnum:]]+\\).+)" nil t)
      (match-string-no-properties 1))))

(defun project-multi--get-build-dir (dir)
  "Get a single build_dir subdir in current DIR.
When there is no valid, subdir, this returns nil.
If there is only one possible build_dir, this will return it.
When there are multiple alternatives, this will ask to the user for
which one to use for this session."
  (unless project-build-dir ;; if project-build-dir skip it because won't be used.
    (let ((build-dir-list
	   (remq nil        ;; Get the list of directories in root with a CMakeCache.txt
		 (mapcar
		  (lambda (dirlist)
		    (and (eq (file-attribute-type (cdr dirlist)) t)
			 (not (string-suffix-p "." (car dirlist)))
			 (file-exists-p (expand-file-name "CMakeCache.txt" (car dirlist)))
			 (car dirlist)))
		  (directory-files-and-attributes dir t nil t 1)))))
      ;; If only one candidate, return it, else ask to the user.
      (if (cdr build-dir-list)
	  (completing-read "Build directory: " build-dir-list nil t)
	(car build-dir-list)))))

;; Utilities functions (a bit less low level) ========================
(defun project-multi--get-plist (dir)
  "Return the plist for DIR from `project-multi--plist'."
  (seq-find (lambda (plist)
	      (string-prefix-p (plist-get plist :root) dir))
	    project-multi--plist))

(defun project-multi--create-plist (dir)
  "Return dbpath for DIR or nil if none."
  (when-let* ((default-directory dir)
	      (root (car (project-multi--get-tree dir))))
    (setq root (concat (file-remote-p default-directory) root))
    (or (project-multi--get-plist root)   ;; already exists
	(car (push (list :project-multi 'cmake
			 :root root
			 :name (project-multi--get-project-name
				(expand-file-name "CMakeLists.txt" root))
			 :compile-command "cmake --build ."
			 :cache nil)
		   project-multi--alist)))))

(defun project-multi--local-plist (dir)
  "Set and return the buffer local value of `project-multi--plist'."
  (if (local-variable-p 'project-multi--plist)
      project-multi--plist
    (project-multi--set-connection-locals)
    (setq-local gtags-mode--plist (or (project-multi--get-plist dir)
				      (project-multi--create-plist dir)))))

;; project integration ===============================================
(defun project-multi-project-backend (dir)
  "Return the project for DIR as an array."
  (project-multi--local-plist dir))

(cl-defgeneric project-build-dir (project)
  "Return build directory of the current PROJECT.

This need to be added lazily because the modeline attempts to call
`project-current' And this function attempts to call `completing-read'
when there are multiple build directories candidates inside the root.
That results in an error."
  (unless (plist-member project :build-dir)
    (setq project (plist-put project
			     :build-dir (project-multi--get-build-dir
					 (plist-get project :root)))))
  (plist-get project :build-dir))

(cl-defmethod project-root ((project (head :project-multi)))
  "Root for PROJECT."
  (plist-get project :root))

(cl-defmethod project-buffers ((project (head :project-multi)))
  "Return the list of all live buffers that belong to PROJECT."
  (mapcar (lambda (buff)
	    (cond
	     ((eq (buffer-local-value 'project-multi--plist buff) project) buff)
	     ((local-variable-p 'project-multi--plist buff) nil)
	     (t (with-current-buffer buff
		  (when (eq (project-multi--local-plist default-directory) project)
		    (current-buffer))))))
	  (buffer-list)))

;;;###autoload
(define-minor-mode project-multi-mode
  "Use Multiple backends for project.el."
  :global t
  :lighter gtags-mode-lighter
  (cond
   (project-multi-mode
    (add-hook 'project-find-functions #'project-multi-project-backend))
   (t
    (remove-hook 'project-find-functions #'project-multi-project-backend))))

(provide 'project-multi-mode)
;;; project-multi-mode.el ends here
