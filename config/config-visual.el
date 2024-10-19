;;; config-visual.el --- Configure the visual experience in Emacs  -*- lexical-binding: t; -*-

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

;; This module configures the visual experience in Emacs.
;; That includes primarily themes and fonts, along with modeline
;; modifications, git gutters, and similar visual aspects of a
;; programming editor.

;;; Code:

(defvar ndb:theme-presets
  '((dark :fontaine dark :theme zenburn)
    (light :fontaine light :theme modus-operandi))
  "Theme presets allows me to switch between themes and fonts.
This is a list of lists of (PRESET-NAME PROPERTY...).

PRESET-NAME is a symbol to identify the preset when switching.

PROPERTTY is one of the following keys and a value:
  :fontaine name of the fontaine preset
  :theme    name of the theme to load.")

(use-package fontaine
  :ensure t
  :demand t
  :commands fontaine-set-preset
  :hook (after-init . fontaine-mode)
  :config
  (setq fontaine-latest-state-file (no-littering-expand-var-file-name "fontaine"))
  (setq fontaine-presets
	'((dark
	   :default-family "Inconsolata"
	   :default-height 140)
	  (light
	   :default-family "Iosevka Slab"
	   :default-height 130
	   :variable-pitch-family "Iosevka Etoile")
	  (t
	   :inherit light))))

;; Themes that I'm going to use: Zenburn and Modus-Operandi
;; Modus Themes are built-in since Emacs 28, and I'm not going to
;; install them separately, as different Emacs versions have different
;; versions of the theme. So it is better to use the built-in theme,
;; rather than worrying about backward-compatibility issues.
(use-package zenburn-theme :ensure t)

;; Theme switching command
;; TODO:: move this to a separate module.

(defun ndb:-theme-preset-names ()
  "Return the list of preset names from `ndb:theme-presets'."
  (delq nil
	(mapcar (lambda (preset) (car preset))
		ndb:theme-presets)))

(defun ndb:-apply-theme-preset (preset)
  "Apply the theme and font specified by PRESET.
This will disable all currently enabled themes and load the theme
specified at the PRESET. Application of font is delegated to
 `fontaine-set-preset'."
  (if (not preset)
      (user-error "nil is not a valid PRESET value. This could be a bug.")
    (let* ((preset-def  (cdr preset))
	   (theme       (plist-get preset-def :theme))
	   (font-preset (plist-get preset-def :fontaine)))
      (mapcar #'disable-theme custom-enabled-themes)
      (load-theme theme :noconfirm)
      (fontaine-set-preset font-preset)))

(defun ndb:-switch-to-theme-prompt ()
  "Prompts the user for a theme preset to enable.
Returns the name of the preset as a symbol."
  (intern
   (completing-read
    (format-prompt "Apply theme from PRESET" nil)
    (ndb:-theme-preset-names)
    nil t)))

(defun ndb:-find-theme-preset-by-name (name)
  "Return the theme preset for `ndb:theme-presets' whose car is
eq to NAME. It returns NIL if no such name found."
  (let ((sym (if (stringp name) (intern name) name)))
    (seq-find (lambda (preset) (eq sym (car preset)))
	      ndb:theme-presets
	      nil)))

(defun ndb:switch-to-theme (preset)
  "Switch to theme specified by PRESET.
PRESET is a car of a list in the `ndb:theme-presets' list."
  (interactive (list (ndb:-switch-to-theme-prompt)))
  (if (and (not (daemonp)) (not window-system))
      (user-error "Cannot switch themes in a terminal with this command. Please try `load-theme'.")
    (ndb:-apply-theme-preset (ndb:-find-theme-preset-by-name preset))))

;; Set the default theme preset
(ndb:switch-to-theme 'light)

;; KEY BIND: set the keybinding for switching theme presets
(keymap-global-set "C-c C-t C-t" #'ndb:switch-to-theme)
					  
(provide 'config-visual)
;;; config-visual.el ends here
