;;; config-programming.el --- Configure programming support  -*- lexical-binding: t; -*-

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

;; This configures the support for programming languages I'm using. Each module
;; under the `programming' directory contains the configurations related to a
;; specific programming language. This file only configures the generic programming
;; support.

;;; Code:

;; Paredit is a must have tool for structal editing and navigation for Lisp-like
;; programming languages. An alternative is parinfer, but I find it slightly diverged
;; from Emacs principles. (I might be wrong...)
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode
	  lisp-mode
	  lisp-interaction-mode)
	 . paredit-mode))

(provide 'config-programming)
;;; config-programming.el ends here
