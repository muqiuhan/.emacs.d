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

;; ----------------------- Generic Configuration -----------------------
(setq-default ocaml-environment t)
(setq-default fsharp-environment t)
(setq-default racket-environment nil)
(setq-default clojure-environment nil)
(setq-default agda-environment nil)
(setq-default coq-environment nil)
(setq-default backup-directory-alist `(("." . "~/.saves")))
(setq-default gc-cons-threshold (* 50 1000 1000))
(setq-default line-spacing 0.2)
(setq-default cursor-type 'bar)
(setq-default font "Consolas Ligaturized v3")
(setq-default font-weight 'bold)
(setq-default font-size 31)
(setq-default chinese-font "TsangerMingHei")
(setq-default chinese-font-weight 'bold)
(setq-default chinese-font-size 31)
(setq-default theme 'modus-vivendi)
(setq-default is-graphics (display-graphic-p))
(setq-default is-x11 (string-equal "x11" (getenv "XDG_SESSION_TYPE")))
(setq-default package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
				 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
				 ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq-default url-proxy-services
              '(("no_proxy" . "^\\(localhost\\|10.*\\)")
                ("http" . "127.0.0.1:7890")
                ("https" . "127.0.0.1:7890")))
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
		 'nano-modeline
		 'eglot
		 'indent-guide
		 'vterm
		 'vterm-toggle
		 'which-key
		 'hide-mode-line
		 'window-numbering
		 'magit
                 'flymake-popon
		 'beacon
		 'rainbow-delimiters
		 'goto-line-preview
                 'corfu
                 'corfu-terminal
                 'cape
		 'go-translate)

(when is-x11
  (require-package 'xclip))

(if is-graphics
    (require-package 'treemacs-all-the-icons
		     'eldoc-box)
  '())


;; ----------------------------------- Basic config -----------------------------------

(toggle-frame-maximized)

(global-auto-revert-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(scroll-bar-mode -1)

(load-theme theme t)
(global-hl-line-mode t)

;; Save your eyes!!!
(if (string-equal "#000000" (face-attribute 'default :background))
    (set-face-attribute 'default nil :background "#191919"))

(set-default 'truncate-lines t)

(when is-graphics
  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil
			:font (format "%s:pixelsize=%d" font english-size)
			:weight 'bold)

    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec
			 :family chinese
			 :size chinese-size
			 :weight chinese-font-weight))))
  (set-font font chinese-font font-size chinese-font-size))

;; ----------------------------------- config -----------------------------------
(use-package corfu
  :defer t
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-auto t
	corfu-popupinfo-delay '(0 . 0)
	corfu-popupinfo-hide nil
	corfu-auto-prefix 0
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
    :defer t
    :init
    (setq cape-dict-case-fold t)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-abbrev)))

;; Flymake
(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :config (setq elisp-flymake-byte-compile-load-path
                (append elisp-flymake-byte-compile-load-path load-path))
  (when is-graphics
    (use-package flymake-popon
      :defer t
      :hook (flymake-mode . flymake-popon-mode)
      :config
      (setq flymake-popon-delay 0.1))))

;; line number
(use-package display-line-numbers
  :defer t
  :config
  (set-face-attribute 'line-number nil
		      :font (face-attribute 'default :font)
		      :weight (face-attribute 'default :weight))

  (set-face-attribute 'line-number-current-line nil
		      :italic nil
		      :font (face-attribute 'default :font)
		      :weight (face-attribute 'default :weight))

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
    :defer t
    :hook (after-init . xclip-mode)))

;; treemacs
(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 35
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

;; eldoc
(when is-graphics
  (use-package eldoc-box
    :defer t
    :hook (eldoc-mode . eldoc-box-hover-mode)
    :config
    (setq-default eldoc-box-offset '(-16 16 50))
    (set-face-attribute 'eldoc-box-border nil :background "#444")
    (set-face-attribute 'eldoc-box-body nil :background (face-attribute 'default :background))))

;; Eglot
(use-package eglot
  :defer t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)))

;; Racket
(when racket-environment
  (require-package 'racket-mode)

  (use-package racket-mode
    :defer t
    :hook (racket-mode . racket-xp-mode)))

;; OCaml
(when ocaml-environment
  (require-package 'utop
		   'tuareg
		   'ocamlformat
		   'dune-format
		   'dune)

  (use-package tuareg
    :defer t
    :commands (ocamlformat-before-save)
    :config
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
    (define-key tuareg-mode-map (kbd "C-I") 'ocamlformat-before-save)))

;; F#
(when fsharp-environment
  (require-package 'fsharp-mode
		   'eglot-fsharp)
  
  (use-package fsharp-mode
    :defer t
    :ensure t))

;; Clojure
(when clojure-environment
  (require-package 'cider)

  (use-package cider
    :defer t
    :ensure t))

;; Agda
(when agda-environment
  (add-hook 'after-init-hook
	    '(lambda ()
	       (interactive)
	       (let ((agda2-program-name "~/.cabal/bin/agda")
		     (agda-mode-locate "~/.cabal/bin/agda-mode locate"))

		 (load-file (let ((coding-system-for-read 'utf-8))
			      (shell-command-to-string agda-mode-locate)))))))

;; Proof Environment for Coq
(when coq-environment
  (require-package 'proof-general
		   'company-coq)

  (use-package proof-general
    :defer t
    :init
    (use-package company-coq
      :defer t
      :hook (coq-mode . company-coq))))

(use-package org-roam-ui
  :bind ("C-c n u" . org-roam-ui-mode)
  :init (when (featurep 'xwidget-internal)
          (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))


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
(use-package nano-modeline
  :defer t
  :init
  (require 'nano-modeline)
  (nano-modeline-text-mode t)

  :config
  (use-package hide-mode-line
    :hook ((completion-list-mode-hook . hide-mode-line-mode)
	   (treemacs-mode . hide-mode-line-mode))
    :init
    (setq-default mode-line-format nil)))

;; Translate
(use-package go-translate
  :bind ("C-c y" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "zh")))

  (setq gts-default-translator
	(gts-translator
	 :picker (gts-prompt-picker)
	 :engines (list (gts-google-engine) (gts-google-rpc-engine))
	 :render (gts-buffer-render))))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; vterm
(use-package vterm
  :defer t
  :bind (([f9] . vterm-toggle)
	 :map vterm-mode-map
	 ([f9] . vterm-toggle)))

(use-package indent-guide
  :defer t
  :hook (after-init . indent-guide-global-mode)
  :config
  (setq indent-guide-delay 0
	indent-guide-character "|")
  (set-face-background 'indent-guide-face (face-attribute 'default :background)))

;; pixel-scroll-mode
(use-package pixel-scroll
  :defer t
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

;; Which key
(use-package which-key
  :defer t
  :init
  (which-key-mode t))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot-fsharp fsharp-mode xclip window-numbering which-key vterm-toggle utop treemacs-all-the-icons solarized-theme rainbow-delimiters racket-mode proof-general ocamlformat nerd-icons nano-modeline markdown-mode magit kind-icon indent-guide hide-mode-line goto-line-preview go-translate flymake-popon eldoc-box dune-format dune corfu-terminal cider cape beacon)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
