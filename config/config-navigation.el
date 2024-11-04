;;; config-navigation.el --- Configure the navigational experience in Emacs  -*- lexical-binding: t; -*-

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

;; This module enhances the navigational experience in Emacs.

;;; Code:

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

;; Vertico with Orderless and Marginalia provide a better minibuffer
;; completion experience. Another alternative to Vertico is Protesilaos'
;; `mct.el', but I'm not sure if it is advance enough to match the experience
;; with Vertico.
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (enable-recursive-minibuffers t)
  :init
  (setopt minibuffer-promp-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; While Vertico enhances minibuffer completion, it is still not complete
;; without a proper filtering method. As far as I know, default vertico
;; has only substring (and similar) completions. What I need is a more fuzzy
;; and rich set of filterations.
(use-package orderless
  :ensure t
  :demand t
  :bind (:map minibuffer-local-completion-map
	      ;; Space is used for separating Orderless search components
	      ;; Do not use it for any keybinding.
	      ("SPC" . nil)
	      ;; ? is a regexp construct.
	      ("?" . nil))
  :config
  (setopt completion-styles '(orderless basic))
  ;; Discard existing completion categories and use mine.
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides
	  '((file             (styles . (basic partial-completion orderless)))
            (buffer           (styles . (basic substring partial-completion orderless)))
            (bookmark         (styles . (basic substring)))
            (imenu            (styles . (basic substring orderless)))
            (consult-location (styles . (basic substring orderless)))
            (kill-ring        (styles . (emacs22 orderless)))))

  ;; Now that I have set orderless for Emacs completion styles,
  ;; set how the orderless should match candidates
  (setopt orderless-matching-styles '(orderless-prefixes
				      orderless-regexp
				      orderless-initialism)))

;; It is nice to have additional information about completion candidates.
;; For example, in `M-x describe-function' it saves a lot of time, when the
;; docstring is displayed with the function name
(use-package marginalia
  :ensure
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
	      ;; cycle through information
	      ("M-a" . marginalia-cycle)))

;; Completion-in-Region, or usual auto-complete behavior with Corfu.
;; Corfu depends on Emacs' bultin completion-at-point-functions rather
;; than providing its own completion backend like Company.
(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :custom
  (tab-always-indent 'complete))

;; With Cosult I can have advance search features, like fuzzy searching for
;; a line in current buffer. There are many other advance searches in Consult.
(use-package consult
  :ensure t
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; Remap some standard Emacs keys to use consult commands
	 ([remap switch-to-buffer] . consult-buffer)
	 ([remap bookmark-jump]    . consult-bookmark)
	 ([remap yank-pop]         . consult-yank-pop)
	 ([remap project-switch-to-buffer] . consult-project-buffer)
	 ;; C-c g bindings for goto commands
	 ("C-c g e" . consult-compile-error)
         ("C-c g f" . consult-flymake)
         ("C-c g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-c g o" . consult-outline)
         ("C-c g m" . consult-mark)
         ("C-c g k" . consult-global-mark)
         ("C-c g i" . consult-imenu)
         ("C-c g I" . consult-imenu-multi)
	 ;; C-c s bindings for searching
	 ("C-c s f" . consult-find)
         ("C-c s c" . consult-locate)
         ("C-c s g" . consult-grep)
         ("C-c s G" . consult-git-grep)
         ("C-c s r" . consult-ripgrep)
         ("C-c s l" . consult-line)
         ("C-c s L" . consult-line-multi)
         ("C-c s k" . consult-keep-lines)
         ("C-c s u" . consult-focus-lines))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-find-args (concat "find . -not ( "
			     "-path */.git* -prune "
			     "-or "
			     "-path */.cache* -prune )")))
	 

(provide 'config-navigation)
;;; config-navigation.el ends here
