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

(setq-default url-proxy-services
	      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
		("http" . "127.0.0.1:7890")
		("https" . "127.0.0.1:7890")))

(setq is-graphics (display-graphic-p))
(setq is-x11 (string-equal "x11" (getenv "XDG_SESSION_TYPE")))

(setq backup-directory-alist `(("." . "~/.saves"))
      gc-cons-threshold (* 50 1000 1000))

(setq-default line-spacing 0.2
	      cursor-type 'hbar)

(when is-graphics
  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil
			:font (format "%s:pixelsize=%d" english english-size)
			:weight 'bold)

    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec
			 :family chinese
			 :size chinese-size
			 :weight 'bold))))
  (set-font "Consolas Ligaturized v3" "TsangerMingHei" 19 19))

;; ----------------------------------- Package config -----------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun require-package (&rest packages)
  (dolist (p packages)
    (unless (package-installed-p p)
      (condition-case nil (package-install p)
        (error
         (package-refresh-contents)
         (package-install p))))))

(require-package 'treemacs
		 'proof-general
		 'company
		 'company-coq
		 'markdown-mode
		 'dune-format
		 'flymake-popon
		 'nano-modeline
		 'racket-mode
		 'eglot
		 'merlin-eldoc
		 'vterm
		 'vterm-toggle
		 'utop
		 'which-key
		 'flycheck-inline
		 'flycheck-ocaml
		 'flycheck
		 'tuareg
		 'hide-mode-line
		 'olivetti
		 'window-numbering
		 'dune
		 'ocamlformat
		 'cider
		 'use-package
		 'magit
		 'beacon
		 'rainbow-delimiters
		 'goto-line-preview
		 'go-translate)

(when is-x11
  (require-package 'xclip))

(if is-graphics
    (require-package 'treemacs-all-the-icons
		     'company-box
		     'eldoc-box)
  (require-package))


;; ----------------------------------- Basic config -----------------------------------

(toggle-frame-maximized)

(global-auto-revert-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(scroll-bar-mode -1)

(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :background "#191919")

;; ----------------------------------- config -----------------------------------

;; company
(use-package company
  :defer t
  :init (global-company-mode t)
  :config
  (when is-graphics
    (use-package company-box
      :hook (company-mode . company-box-mode)
      :config
      (setq-default company-box-doc-delay 0
		    company-box-doc-delay 0
		    company-box-doc-enable t
		    company-box-doc-frame-parameters
		    '((vertical-scroll-bars . nil)
		      (horizontal-scroll-bars . nil)
		      (internal-border-width . 1)
		      (left-fringe . 0)
		      (right-fringe . 0))))))

;; Flymake
(use-package flymake
  :defer t
  :init
  (when is-graphics
    (use-package flymake-popon
      :hook (flymake-mode . flymake-popon-mode)
      :config
      (setq flymake-popon-delay 0
	    flymake-popon-posframe-border-width 1)

      (set-face-attribute 'flymake-popon-posframe-border nil :foreground "#444444"))))

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
  (setq beacon-color "#0f0"))

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
    (set-face-attribute face nil
			:font (face-attribute 'default :font)
			:height 130))
  
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
    (setq-default eldoc-box-offset '(-16 16 16))
    (set-face-attribute 'eldoc-box-border nil :background "#444")
    (set-face-attribute 'eldoc-box-body nil :background (face-attribute 'default :background))))

;; Eglot
(use-package eglot
  :defer t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)))

;; Flycheck
(use-package flycheck-inline
  :defer t
  :hook (flycheck-mode . flycheck-inline-mode))

;; Racket
(use-package racket-mode
  :defer t
  :hook (racket-mode . racket-xp-mode))

;; OCaml
(use-package tuareg
  :defer t
  :hook ((tuareg-mode . flycheck-mode)
	 (tuareg-mode . eglot-mode))
  :config
  (use-package merlin
    :hook ((tuareg-mode . merlin-mode)
	   (merlin-mode . merlin-eldoc-setup))

    :config
    (setq merlin-completion-with-doc t))

  (use-package flycheck-ocaml
    :ensure t
    :config
    (add-hook 'tuareg-mode-hook
              (lambda ()
		(setq-local merlin-error-after-save nil)
		(flycheck-ocaml-setup))))
  
  (use-package ocamlformat
    :config
    (define-key tuareg-mode-map (kbd "C-x x f") 'ocamlformat-before-save)))

;; Clojure
(use-package cider
  :defer t
  :ensure t)

;; Agda
(add-hook 'after-init-hook
	  '(lambda ()
	     (interactive)
	     (let ((agda2-program-name "~/.cabal/bin/agda")
		   (agda-mode-locate "~/.cabal/bin/agda-mode locate"))

	       (load-file (let ((coding-system-for-read 'utf-8))
			    (shell-command-to-string agda-mode-locate))))))

;; Proof Environment for Coq
(use-package proof-general
  :defer t
  :init
  (use-package company-coq
    :defer t
    :hook (coq-mode . company-coq)))

;; window numbering
(use-package window-numbering
  :defer t
  :hook ((after-init . window-numbering-mode)
	 (window-numbering-mode . window-numbering-clear-mode-line)))

;; Markdown
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . olivetti-mode)

  :init
  (setq auto-mode-alist (append '(("README" . olivetti-mode)) auto-mode-alist)))

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
;; init.el ends here
