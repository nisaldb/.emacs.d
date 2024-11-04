;;; config-clojure.el --- Configure Clojure Support  -*- lexical-binding: t; -*-

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

;; This file configures the support for programming in Clojure

;;; Code:

;; Checker for clj-kondo.
;; This is needed to be loaded before clojure-mode
(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

;; To complement the refactoring features provided by Clojure-mode
;; and CIDER
(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c r"))

;; Since there could be java classes in a Clojure source code,
;; it is useful to enable CamelCase support for editing commands.
(use-package subword-mode
  :ensure nil
  :hook (clojure-mode . subword-mode))

;; CIDER is the SLIME for Clojure
(use-package cider
  :ensure t
  :pin melpa
  :config
  (setopt cider-repl-display-help-banner nil)
  (setopt cider-connection-message-fn #'cider-random-tip)
  (setopt nrepl-hide-special-buffers t))

(provide 'config-clojure)
;;; config-clojure.el ends here
