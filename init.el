;;; init.el --- My Emacs Configuration Tree          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nisal Bandara

;; Author: Nisal Bandara <thatndb@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
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

;; TODO:: write the commentary

;;; Code:

(defvar ndb:root-dir user-emacs-directory
  "Root directory of this configuration tree.")

(defvar ndb:configs-dir (expand-file-name "config/" ndb:root-dir)
  "Directory of custom configuration modules.
This directory and all its sub directories are added to the `load-path'.")

(defvar ndb:libs-dir (expand-file-name "lib/" ndb:root-dir)
  "Utility functions and variables that are used for configurations.")

(defvar ndb:extra-dir (expand-file-name "extra/" ndb:root-dir)
  "Extra packages, that happen to be distributed outside of any package archive.
This directory is not added to the `load-path', only its subdirectories.")

;; This is the first utility function of this configuration.
;; Ideally it should be in a module under the `ndb:lib-dir' directory,
;; but since the load path is not configured yet, It has to be here.
;; 
;; TODO:: this implementation violates open/close principle. Is there
;; a better way to do this?
(defun ndb:add-directory-to-load-path (dir &optional depth)
  "Add a DIR and/or its subdirectories to the `load-path'.
Value of DEPTH, determine whether subdirectories and the DIR should be added to
the load path.

if DEPTH is `'no-subdirs' or nil, only DIR is added to the load path.

if DEPTH is `'all-dirs', DIR and all its subdirectories are added to the
load path recursively.

if DEPTH is `'all-subdirs', DIR will not added to the load path, but all
its subdirectories (recursively) will be.

if DEPTH is `'only-subdirs', DIR will not be added and only the immediate
subdirs will be added to the load path."
  (cond ((eq depth 'all-dirs)
	 (ndb:add-directory-to-load-path dir 'only-subdirs)
	 (ndb:add-directory-to-load-path dir))
	((or (not depth) (eq depth 'no-subdirs))
	 (add-to-list 'load-path dir))
	((or (eq depth 'all-subdirs)
	     (eq depth 'only-subdirs))
	 (dolist (afile (directory-files dir))
	   (let ((dirname (expand-file-name afile dir))
		 (recurse-depth (if (eq depth 'only-subdirs) 'no-subdirs 'all-dirs)))
	     (when (and (file-directory-p dirname)
			(not (string-prefix-p "." afile)))
	       (ndb:add-directory-to-load-path dirname recurse-depth)))))))

;; Now add dirs to the load path
(ndb:add-directory-to-load-path ndb:configs-dir 'all-dirs)
(ndb:add-directory-to-load-path ndb:libs-dir 'no-subdirs)
(ndb:add-directory-to-load-path ndb:extra-dir 'only-subdirs)

;; Set the package archives.
;; This is needed because melpa and melpa-stable are not included in Emacs
;; by default.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("melpa-stable" . 70)
                                                      ("melpa"  . 60)))

;; I could have used `use-package' to ensure this automatically.
;; But I would like to treat it as a built-in Emacs module.
(require 'no-littering)

;; I'm not completely sure about enable backup files.
;; Because (a) there is a risk of exposing sensitive information through those
;; backup files and (b) backup files pollutes my project directories.
;; However, backup files are generally a safer mechanism to prevent accidental
;; data loss. Given how clumsy I am, disabling it may not be good for me.
;;
;; no-littering package can solve the project directory pollution by placing
;; all the backup files in a separate directory at `user-emacs-directory'.
;; This will consequently help me to solve the security problem also; just
;; deleting the backup directory will be enough to delete all the backup files.
(no-littering-theme-backups)

;; Use the same backup policy for TRAMP too.
(setopt tramp-backup-directory-alist backup-directory-alist)

;; Backup should be created by copying. Otherwise hard links will
;; refer to the wrong file.
(setopt backup-by-copying             t)
(setopt backup-by-copying-when-linked t)

;; Use numbered backup files. Otherwise it would be meaningless for my use case.
;; However limit the number of backup files by keeping only oldest and newest 10.
;; Number 10 is just a guess.
(setopt version-control t)
(setopt kept-old-versions 10)
(setopt kept-new-versions 10)

;; Put custom file in the `etc' directory, maintained by no-littering.
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; I used to load a dashboard at the Emacs startup. However, I found no real
;; benefit in doing this. I used it to shortcut into my projects, but other than
;; that it was simply another package to configure. Now I would like to drop into
;; a scratch buffer.
(setopt initial-buffer-choice t) ;; this will inhibit the startup screen too.

;; For macOS, use COMMAND key as META, and OPTION key as SUPER.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
	mac-command-modifier 'meta))

;; Set my username and email
(setopt user-full-name "Nisal Bandara")
(setopt user-mail-address "devel@2path.org")


(provide 'init)
;;; init.el ends here
