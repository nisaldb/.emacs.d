;;; config-c.el --- Configure C/C++ modes            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nisal Bandara

;; Author: Nisal Bandara <thatndb@gmail.com>
;; Keywords: c, convenience

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

;; These are my configurations for C and C++ programming language.
;; Actually, I don't write in C++, but by nature most of these
;; configurations will apply to C++ programming also.
;; I only use C for my personal projects. So I would like to keep
;; these configurations minimal or old-school. For example, I would
;; never use LSP for C programming. I like to maintain tag databases
;; for source code navigation. These preferences might seems a bit arcane
;; but for some reason I can't shake them off.

;;; Code:

;; Add a new style for PostgreSQL developments. This is an excerpt from
;; PostgreSQL's `src/tools/editor/emacs.sample' file.
(c-add-style "postgresql"
	     '("bsd"
	       (c-auto-align-backslashes . nil)
	       (c-basic-offset . 4)
	       (c-offsets-alist . ((case-label . -4)
				   (label . -)
				   (statement-case-open . +)))
	       (fill-column . 78)
	       (indent-tabs-mode . t)
	       (tab-width . 4)))

;; The builtin "BSD" style is not the same as style define
;; in FreeBSD's style(9) page. Hence a new style need to be
;; defined to match it.
(c-add-style "freebsd"
	     '("bsd"
	       (fill-column . 80)
	       (indent-tabs-mode . t)
	       (tab-width . 8)
	       (c-offsets-alist . ((defun-block-intro . +)
				   (statement-case-intro . +)
				   (substatement . +)
				   (arglist-cont-nonempty . *)
				   (inclass . +)))))

;; Set default C style to "freebsd".
(setopt c-default-style '((c-mode . "freebsd")
			  (java-mode . "java")
			  (awk-mode . "awk")
			  (other . "gnu")))

;; Modify Emacs in accordance with the PostgreSQL style for
;; relevant source files.
(defun ndb:postgresql-c-mode ()
  "Activate postgresql releated configuration whenever a C file from PostgreSQL source is opened."
  (when (string-match "/postgres\\(ql\\)?/" buffer-file-name)
    (c-set-style "postgresql")))

;; C mode hooks
(add-hook 'c-mode-hook #'ndb:postgresql-c-mode)

;; I will not use any third-party packages for C programming.
;; The rationale is that I must use what is available in Emacs,
;; until I found a concrete reason to use a feature that is
;; not available in Emacs.

(provide 'config-c)
;;; config-c.el ends here
