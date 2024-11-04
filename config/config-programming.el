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

;; This configures the support for programming languages I'm using.  Each module
;; under the `programming' directory contains the configurations related to a
;; specific programming language.  This file only configures the generic programming
;; support.

;;; Code:

(defvar ndb:personal-dev-dir-pattern "/Personal/"
  "A pattern to identify directories which holds the personal coding projects.")

;; Smartparens is a better alternative for Paredit. It supports languages other
;; than lisps and has better keybindings.
;; While configuring this I found a caveat in macOS, where it doesn't let me to
;; use `C-M-d' as a keybinding. This is because that key sequence is already
;; binded for dictionary lookup. To fix this following command need to be
;; executed followed by a restart:
;;
;; defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys \
;;   -dict-add 70 '<dict><key>enabled</key><false/></dict>'
;;
(use-package smartparens
  :ensure t
  :pin melpa-stable
  :hook ((prog-mode text-mode) . smartparens-mode)
  :custom
  (sp-base-key-bindings 'sp)
  :config
  (require 'smartparens-config))

(require 'config-c)

(provide 'config-programming)
;;; config-programming.el ends here
