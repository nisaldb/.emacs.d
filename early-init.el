;;; early-init.el --- Early initializations  -*- lexical-binding: t; -*-

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

;; `early-init.el' is usually used to customize the initialization of
;; package system or the GUI.  But here I have set some variables that
;; are related to native-compilation.  I'm not sure if they can be moved
;; to the `init.el', so I'm going to keep them here.

;;; Code:

;; Prefer loading newer compiled files
(setopt load-prefer-newer t)

;; Suppress compiler warnings
(setopt native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
;;; early-init.el ends here
