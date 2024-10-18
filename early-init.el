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
;; package system or the GUI.

;;; Code:

;; Prefer loading newer compiled files
(setopt load-prefer-newer t)

;; Suppress compiler warnings
(setopt native-comp-async-report-warnings-errors 'silent)

;; Resize the frame pixelwise, do not round the frame size to match
;; character height.
(setopt frame-resize-pixelwise t)

;; Disable some UI clutter
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here
