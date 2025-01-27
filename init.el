;;; init.el --- A lightweight, fast, simple and crude configuration -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Copyright (C) 2022 Muqiu Han <muqiu-han@outlook.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A lightweight, fast, simple and crude configuration for GNU Emacs.
;;
;;
;;; Code:

;;; Startup optimizations
;; Set garbage collection threshold

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set deferred timer to reset them
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;; ----------------------- Generic Configuration -----------------------
(setq-default ocaml-environment nil)
(setq-default c++-environment nil)
(setq-default fsharp-environment nil)
(setq-default racket-environment nil)
(setq-default scala-environment nil)
(setq-default rescript-environment t)
(setq-default rust-environment nil)
(setq-default clojure-environment nil)
(setq-default agda-environment nil)
(setq-default evil nil)
(setq-default ement nil)
(setq-default coq-environment nil)
(setq-default backup-directory-alist `(("." . "~/.saves")))
(setq-default line-spacing 0)
(setq-default cursor-type 'bar)
(setq-default font "FrankMono")
(setq-default font-weight 'bold)
(setq-default font-size 115)
(setq-default font-ligature t)
(setq-default minimap nil)
(setq-default chinese-font "FrankMono")
(setq-default chinese-font-weight 'bold)
(setq-default chinese-font-size 115)
(setq-default light-theme 'modus-operandi)
(setq-default dark-theme 'modus-vivendi)
(setq-default hl-line t)
(setq-default tab-bar nil)
(setq-default display-line-numbers-retro nil)
(setq-default relative-line-number nil)
(setq-default is-graphics (display-graphic-p))
(setq-default is-x11 (string-equal "x11" (getenv "XDG_SESSION_TYPE")))
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(setq-default url-proxy-services
              '(("no_proxy" . "^\\(localhost\\|10.*\\)")
                ("http" . "127.0.0.1:20172")
                ("https" . "127.0.0.1:20172")))
;; ----------------------------------- Package config -----------------------------------

(require 'package)

(defun require-package (&rest packages)
  (dolist (p packages)
    (unless (package-installed-p p)
      (condition-case nil (package-install p)
        (error
         (package-refresh-contents)
         (package-install p))))))

(require-package 'treemacs
		 'markdown-mode
		 'telephone-line
		 'eglot
		 'vterm
		 'vterm-toggle
		 'doom-themes
		 'vc-msg
		 'which-key
		 'multiple-cursors
		 'hide-mode-line
		 'window-numbering
		 'magit
		 'projectile
		 'rescript-mode
                 'sideline-flymake
		 'beacon
		 'indent-guide
		 'rainbow-delimiters
		 'goto-line-preview
                 'corfu
                 'corfu-terminal
                 'cape
		 'markdown-soma
		 'all-the-icons-nerd-fonts
		 'treemacs-all-the-icons
		 'ligature
		 'eldoc-box
		 'indent-guide
		 'centered-cursor-mode
		 'go-translate)

(when is-x11
  (require-package 'xclip))

;; ----------------------------------- Basic config -----------------------------------

(toggle-frame-maximized)
(global-auto-revert-mode 1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(when is-graphics
  (tool-bar-mode -1)
  (fringe-mode -1)
  (scroll-bar-mode -1)
  (tab-line-mode -1))

(when tab-bar 
  (use-package tab-bar
    :hook (window-setup . tab-bar-mode)
    :config
    (setq tab-bar-separator ""
	  tab-bar-new-tab-choice "*scratch*"
	  tab-bar-tab-name-truncated-max 20
	  tab-bar-auto-width nil
	  tab-bar-close-button-show nil
	  tab-bar-tab-hints t)

    (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

    (setq tab-bar-tab-name-function
          (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
			    (count (length (window-list-1 nil 'nomini)))
			    (truncated-tab-name (if (< (length raw-tab-name)
                                                       tab-bar-tab-name-truncated-max)
						    raw-tab-name
						  (truncate-string-to-width raw-tab-name
									    tab-bar-tab-name-truncated-max
									    nil nil tab-bar-tab-name-ellipsis))))
                       (if (> count 1)
			   (concat truncated-tab-name "(" (number-to-string count) ")")
			 truncated-tab-name))))

    (setq tab-bar-tab-name-format-function
          (lambda (tab i)
            (let ((face (funcall tab-bar-tab-face-function tab)))
              (concat
               (propertize " " 'face face)
               (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
               (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

    (tab-bar--update-tab-bar-lines)

    (when (daemonp)
      (add-hook 'after-make-frame-functions
		#'(lambda (&rest _) (force-mode-line-update))))))

(when (string= system-type "gnu/linux")
  (defun theme--handle-dbus-event (a setting values)
    "Handler for FreeDesktop theme changes."
    (when (string= setting "ColorScheme")
      (let ((scheme (car values)))
        (cond
         ((string-match-p "Dark" scheme)
          (load-theme dark-theme t))
         ((string-match-p "Light" scheme)
          (load-theme light-theme t))
         (t (message "I don't know how to handle scheme: %s" scheme))))))

  (require 'dbus)
  (dbus-register-signal :session
                        "org.freedesktop.portal"
                        "/org/freedesktop/portal/desktop"
                        "org.freedesktop.impl.portal.Settings"
                        "SettingChanged"
                        #'theme--handle-dbus-event)


  (if (and is-graphics (eq 1 (caar (dbus-ignore-errors
				     (dbus-call-method
				      :session
				      "org.freedesktop.portal.Desktop"
				      "/org/freedesktop/portal/desktop"
				      "org.freedesktop.portal.Settings" "Read"
				      "org.freedesktop.appearance" "color-scheme")))))
      (if dark-theme
	  (load-theme dark-theme t)
	(when light-theme (load-theme light-theme t)))
    (load-theme dark-theme t)))

(set-default 'truncate-lines t)

(when is-graphics
  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil
			:font font
			:height font-size
			:weight font-weight)
    
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec
			 :family chinese
			 :height chinese-size
			 :weight chinese-font-weight))))
  
  (set-font font chinese-font font-size chinese-font-size)

  (use-package ligature
    :config
    (ligature-set-ligatures 't '("www"))
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
					 ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
					 "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
					 "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
					 "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
					 "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
					 "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
					 "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
					 ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
					 "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
					 "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
					 "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
					 "\\\\" "://"))
    (when font-ligature
      (global-ligature-mode t))))

(set-face-attribute 'font-lock-keyword-face nil :font (face-attribute 'default :font))
(set-face-attribute 'font-lock-function-name-face nil :font (face-attribute 'default :font))

;; ----------------------------------- config -----------------------------------
(use-package corfu
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config

  (setq corfu-auto t
	corfu-popupinfo-delay '(0 . 0)
	corfu-popupinfo-hide nil
	corfu-auto-prefix 3
	corfu-quit-at-boundary t
	corfu-quit-no-match t
	corfu-preview-current t
	corfu-preselect 'prompt
	corfu-scroll-margin 5
	corfu-echo-mode t
	corfu-auto-delay 0)

  (unless is-graphics
    (use-package corfu-terminal
      :hook (global-corfu-mode . corfu-terminal-mode)))
  
  (when is-graphics
    (require-package 'nerd-icons
		     'kind-icon)
    (use-package kind-icon
      :after corfu
      :init
      (use-package nerd-icons
	:config
	(defconst corfu-kind-icon-mapping
	  `((array . ,(nerd-icons-codicon "nf-cod-symbol_array" :face 'font-lock-type-face))
	    (boolean . ,(nerd-icons-codicon "nf-cod-symbol_boolean" :face 'font-lock-builtin-face))
	    (class . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'font-lock-type-face))
	    (color . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success) )
	    (command . ,(nerd-icons-codicon "nf-cod-terminal" :face 'default) )
	    (constant . ,(nerd-icons-codicon "nf-cod-symbol_constant" :face 'font-lock-constant-face) )
	    (constructor . ,(nerd-icons-codicon "nf-cod-triangle_right" :face 'font-lock-function-name-face) )
	    (enummember . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
	    (enum-member . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
	    (enum . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'font-lock-builtin-face) )
	    (event . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'font-lock-warning-face) )
	    (field . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-variable-name-face) )
	    (file . ,(nerd-icons-codicon "nf-cod-symbol_file" :face 'font-lock-string-face) )
	    (folder . ,(nerd-icons-codicon "nf-cod-folder" :face 'font-lock-doc-face) )
	    (interface . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'font-lock-type-face) )
	    (keyword . ,(nerd-icons-codicon "nf-cod-symbol_keyword" :face 'font-lock-keyword-face) )
	    (macro . ,(nerd-icons-codicon "nf-cod-symbol_misc" :face 'font-lock-keyword-face) )
	    (magic . ,(nerd-icons-codicon "nf-cod-wand" :face 'font-lock-builtin-face) )
	    (method . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
	    (function . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
	    (module . ,(nerd-icons-codicon "nf-cod-file_submodule" :face 'font-lock-preprocessor-face) )
	    (numeric . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'font-lock-builtin-face) )
	    (operator . ,(nerd-icons-codicon "nf-cod-symbol_operator" :face 'font-lock-comment-delimiter-face) )
	    (param . ,(nerd-icons-codicon "nf-cod-symbol_parameter" :face 'default) )
	    (property . ,(nerd-icons-codicon "nf-cod-symbol_property" :face 'font-lock-variable-name-face) )
	    (reference . ,(nerd-icons-codicon "nf-cod-references" :face 'font-lock-variable-name-face) )
	    (snippet . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face) )
	    (string . ,(nerd-icons-codicon "nf-cod-symbol_string" :face 'font-lock-string-face) )
	    (struct . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'font-lock-variable-name-face) )
	    (text . ,(nerd-icons-codicon "nf-cod-text_size" :face 'font-lock-doc-face) )
	    (typeparameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
	    (type-parameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
	    (unit . ,(nerd-icons-codicon "nf-cod-symbol_ruler" :face 'font-lock-constant-face) )
	    (value . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-builtin-face) )
	    (variable . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face) )
	    (t . ,(nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face))))

	(defsubst nerd-icon--metadata-get (metadata type-name)
	  "Get METADATA for keyword TYPE-NAME from the completion properties."
	  (or
	   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
	   (cdr (assq (intern type-name) metadata))))

	(defsubst nerd-icon-formatted (kind)
	  "Get icon for KIND."
	  (let* ((icon (alist-get kind corfu-kind-icon-mapping))
		 (icon-face (get-text-property 0 'face icon))
		 (icon-bg (plist-get icon-face :inherit))
		 (icon-pad (propertize " " 'face (append '(:height 0.5) icon-bg)))
		 (item-pad (propertize " " 'face '(:height 0.5))))
	    (concat icon-pad icon icon-pad item-pad)))

	(defun nerd-icon-margin-formatter (metadata)
	  (if-let ((kind-func (nerd-icon--metadata-get metadata "company-kind")))
              (lambda (cand)
		(if-let ((kind (funcall kind-func cand)))
		    (nerd-icon-formatted kind)
		  (nerd-icon-formatted t)))))

	(add-to-list 'corfu-margin-formatters #'nerd-icon-margin-formatter))))

  ;; Add extensions
  (use-package cape
    :init
    (setq cape-dict-case-fold t)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-abbrev)))

;; Evil
(when evil
  (require-package 'evil)
  (use-package evil
    :init
    (evil-mode 1)))

;; Centered cursor
(unless is-graphics
  (use-package centered-cursor-mode
    :hook (after-init . global-centered-cursor-mode)))

;; Flymake
(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :config (setq elisp-flymake-byte-compile-load-path
                (append elisp-flymake-byte-compile-load-path load-path))

  (use-package sideline
    :hook (flymake-mode . sideline-mode)
    :init
    (setq sideline-flymake-display-mode 'point)
    (setq sideline-backends-right '(sideline-flymake))))

;; multiple-cursors
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-M-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-M-p") 'mc/mark-previous-like-this))

;; Projectile
(use-package projectile
  :defer t
  :bind ("C-x K" . projectile-kill-buffers))

(when (and is-graphics minimap)
  (require-package 'minimap)
  (use-package minimap
    :config
    (setq minimap-update-delay 0
	  minimap-width-fraction 0
	  minimap-hide-scroll-bar t)

    :hook (after-init . minimap-mode)))

;; line number
(use-package display-line-numbers
  :defer t
  :config

  (when relative-line-number (setq display-line-numbers 'relative))

  ;; Retro!!!
  (when display-line-numbers-retro
    (set-face-attribute 'line-number nil
			:italic nil
			:background "#00a"
			:foreground "#ccc"
			:font (face-attribute 'default :font)
			:weight (face-attribute 'default :weight))

    (set-face-attribute 'line-number-current-line nil
			:italic nil
			:background "#00f"
			:foreground "#fff"
			:font (face-attribute 'default :font)
			:weight (face-attribute 'default :weight)))

  :hook (prog-mode . display-line-numbers-mode))

;; Beacon
(use-package beacon
  :defer t
  :hook (after-init . beacon-mode)
  :config
  (setq beacon-color (face-attribute 'default :foreground)))

;; xclip: easy to synchorize with the system clipboard
(when is-x11
  (use-package xclip
    :hook (after-init . xclip-mode)))

;; Evil
(when evil
  (require-package 'evil)
  (use-package evil
    :hook (after-init . evil-mode)))

;; treemacs
(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 45
	treemacs-indentation 2
	treemacs-position 'left
	treemacs-icon-tag-leaf "0")

  (dolist (face '(treemacs-root-face
                  treemacs-git-unmodified-face
                  treemacs-git-modified-face
                  treemacs-git-renamed-face
                  treemacs-git-ignored-face
                  treemacs-git-untracked-face
                  treemacs-git-added-face
                  treemacs-git-conflict-face
                  treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-tags-face))
    (set-face-attribute face nil :font (face-attribute 'default :font)))

  :init
  (when (display-graphic-p)
    (use-package treemacs-all-the-icons)
    (treemacs-load-theme "all-the-icons"))

  :bind (([f8]        . treemacs)
         ("M-RET"       . treemacs-select-window)
         ("C-x t t"   . treemacs)))

;; Eglot
(use-package eglot
  :defer t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)))

(when c++-environment
  (require-package 'clang-format)
  (use-package cc-mode
    :config
    (define-key c++-mode-map (kbd "C-I") 'clang-format-buffer)
    (define-key c-mode-map (kbd "C-I") 'clang-format-buffer)))

;; Racket
(when racket-environment
  (require-package 'racket-mode)

  (use-package racket-mode
    :hook (racket-mode . racket-xp-mode)))

;; OCaml
(when ocaml-environment
  (require-package 'utop
		   'tuareg
		   'ocamlformat
		   'dune-format
		   'dune)

  (use-package tuareg
    :commands (ocamlformat-before-save)
    :init
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
    :config
    (define-key tuareg-mode-map (kbd "C-I") 'ocamlformat-before-save)))

;; Rescript
(use-package rescript-mode
  :defer t
  :hook ((rescript-mode . (lambda () (electric-indent-local-mode -1))))
  :config
  (require 'eglot)
  (add-to-list 'eglot-server-programs
               '(rescript-mode . ("rescript-language-server" "--stdio"))))

;; F#
(when fsharp-environment
  (require-package 'fsharp-mode
		   'ob-fsharp
		   'eglot-fsharp)
  
  (use-package fsharp-mode
    :ensure t
    :config
    (use-package eglot-fsharp)
    (use-package ob-fsharp)))

;; Clojure
(when clojure-environment
  (require-package 'cider)

  (use-package cider
    :ensure t))

;; Clojure
(when scala-environment
  (require-package 'scala-mode
		   'sbt-mode)

  (use-package scala-mode
    :interpreter ("scala" . scala-mode)
    :config
    (use-package sbt-mode
      :commands sbt-start sbt-command
      :config
      (substitute-key-definition
       'minibuffer-complete-word
       'self-insert-command
       minibuffer-local-completion-map)
      (setq sbt:program-options '("-Dsbt.supershell=false")))))

;; Agda
(when agda-environment
  (add-hook 'after-init-hook
	    '(lambda ()
	       (interactive)
	       (let ((agda2-program-name "~/.cabal/bin/agda")
		     (agda-mode-locate "~/.cabal/bin/agda-mode locate"))

		 (load-file (let ((coding-system-for-read 'utf-8))
			      (shell-command-to-string agda-mode-locate)))))))
(setq-default agda2-program-name "~/.cabal/bin/agda")

;; Coq
(when coq-environment
  (require-package 'proof-general
		   'company-coq)

  (use-package proof-general
    :init
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
    
    (use-package company-coq
      :defer t
      :hook (coq-mode . company-coq))))

;; Rust
(when rust-environment
  (require 'eglot)
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))

;; window numbering
(use-package window-numbering
  :defer t
  :hook ((after-init . window-numbering-mode)
	 (window-numbering-mode . window-numbering-clear-mode-line)))

;; Goto line preview
(use-package goto-line-preview
  :defer t
  :init
  (global-set-key [remap goto-line] 'goto-line-preview))

;; modeline
(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

  (setq telephone-line-height 24
	telephone-line-evil-use-short-tag t)
  
  (use-package hide-mode-line
    :hook ((completion-list-mode-hook . hide-mode-line-mode)
	   (treemacs-mode . hide-mode-line-mode))
    :init
    (setq-default mode-line-format nil))

  (telephone-line-mode 1))

;; Translate
(use-package go-translate
  :bind ("C-c y" . gt-do-translate)
  :config
  (setq gt-langs '(en zh))

  (setq gt-default-translator
	(gt-translator
	 :engines (list (gt-google-engine) (gt-google-rpc-engine))
	 :render (gt-buffer-render))))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; vterm
(use-package vterm
  :defer t
  :bind (("C-`" . vterm-toggle)
	 :map vterm-mode-map ([f9] . vterm-toggle)))

;; indent guides
(use-package indent-guide
  :defer t
  :hook (after-init . indent-guide-global-mode))

;; pixel-scroll-mode
(when is-graphics
  (use-package pixel-scroll
    :hook (after-init . pixel-scroll-precision-mode)
    :config
    (setq pixel-scroll-precision-interpolate-page t)

    (defun +pixel-scroll-interpolate-down (&optional lines)
      (interactive)
      (if lines
	  (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
	(pixel-scroll-interpolate-down)))

    (defun +pixel-scroll-interpolate-up (&optional lines)
      (interactive)
      (if lines
	  (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
      (pixel-scroll-interpolate-up))

    (defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
    (defalias 'scroll-down-command '+pixel-scroll-interpolate-up))
  :init
  (setq redisplay-dont-pause t
	scroll-margin 1
	scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position 1))

;; Which key
(use-package which-key
  :defer t
  :init
  (which-key-mode t))

;; hl-line-mode
(when hl-line
  (use-package hl-line
    :hook (prog-mode . hl-line-mode)))

;; eldoc
(if is-graphics
    (use-package eldoc-box
      :defer t
      :hook (eldoc-mode . eldoc-box-hover-mode)
      :config
      (setq-default eldoc-box-offset '(-16 16 50))
      (set-face-attribute 'eldoc-box-border nil :background "#444")
      (set-face-attribute 'eldoc-box-body nil :background (face-attribute 'default :background)))
  (global-eldoc-mode -1))

;; Ement
(when ement
  (use-package ement
    :hook (ement-mode .
		      (lambda ()
			(interactive)
			(setq mode-line-format nil)
			(indent-guide-mode -1)))))

;; Markdown
(use-package markdown-soma
  :config
  (setq markdown-soma--render-buffer-hooks
	'(after-revert-hook
	  after-save-hook
	  after-change-functions
	  post-command-hook)))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here.
