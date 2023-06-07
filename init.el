;;; init.el --- A lightweight, fast, simple and crude configuration -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (c) 2022 Muqiu Han

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
;; OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; A lightweight, fast, simple and crude configuration for GNU Emacs.
;;

;;; Code:

;; ----------------------------------- Package config -----------------------------------

(require 'package)
(setq-default url-proxy-services
	      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
		("http" . "127.0.0.1:7890")
		("https" . "127.0.0.1:7890")))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun require-package (&rest packages)
  (dolist (p packages)
    (unless (package-installed-p p)
      (condition-case nil (package-install p)
        (error
         (package-refresh-contents)
         (package-install p))))))

(require-package 'treemacs
		 'xclip
		 'proof-general
		 'company
		 'company-coq
		 'markdown-mode
		 'dune-format
		 'darkroom
		 'doom-themes
		 'racket-mode
		 'utop
		 'which-key
		 'tuareg
		 'window-numbering
		 'treemacs-all-the-icons
		 'dune
		 'ocamlformat
		 'use-package
		 'fsharp-mode
		 'magit
		 'toml
		 'beacon
		 'goto-line-preview
		 'youdao-dictionary)


;; ----------------------------------- Basic config -----------------------------------
(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (fringe-mode -1)
  (scroll-bar-mode -1)

  (set-face-attribute 'default nil
		      :font "SF Mono"
		      :weight 'semibold
		      :height 115))

(setq-default line-spacing 0.0
	      cursor-type '(hbar . 5))

(set-cursor-color "#0f0")

(global-auto-revert-mode 1) ;; auto revert/refresh file when change detected
(setq backup-directory-alist `(("." . "~/.saves"))) ;; set the unified storage path for backup files

(setq gc-cons-threshold (* 50 1000 1000))
(load-theme 'doom-gruvbox t)

;; ----------------------------------- config -----------------------------------

;; company
(use-package company
  :defer t
  :init (global-company-mode t))

;; line number
(use-package display-line-numbers
  :defer t

  :init
  (set-face-attribute 'line-number nil :italic nil)
  (set-face-attribute 'line-number-current-line nil :italic nil)
  
  :hook (prog-mode . display-line-numbers-mode))

;; xclip: easy to synchorize with the system clipboard
(use-package xclip
  :defer t
  :init
  (when (string-equal "x11" (getenv "XDG_SESSION_TYPE"))
    (xclip-mode)))

;; treemacs
(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 35
	treemacs-indentation 2
	treemacs-position 'right
	treemacs-icon-tag-leaf "0")
  :init
  (when (display-graphic-p)
    (use-package treemacs-all-the-icons)
    (treemacs-load-theme "all-the-icons"))

  :bind (([f8]        . treemacs)
         ("M-RET"       . treemacs-select-window)
         ("C-x t t"   . treemacs)))

;; powerline
(use-package powerline
  :defer t
  :hook (after-init . powerline-center-theme))

;; Racket
(use-package racket-mode
  :defer t
  :hook (racket-mode . racket-xp-mode))

;; OCaml
(use-package tuareg
  :defer t
  :hook (tuareg-mode . merlin-mode)
  :config
  (define-key tuareg-mode-map (kbd "C-x x f") 'ocamlformat))

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
  :hook (after-init . window-numbering-mode))

;; Markdown
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . darkroom-mode))

(use-package darkroom
  :defer t
  :config
  (setq darkroom-margin-increment 20))

;; Add README support
(setq auto-mode-alist (append '(("README" . darkroom-mode)) auto-mode-alist))

;; Goto line preview
(use-package goto-line-preview
  :defer t
  :init
  (global-set-key [remap goto-line] 'goto-line-preview))

;; Translate
(use-package youdao-dictionary
  :defer t
  :init
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point-posframe)
  (global-set-key (kbd "C-c p") 'youdao-dictionary-play-voice-at-point))

;; Which key
(use-package which-key
  :defer t
  :init
  (which-key-mode t))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(proof-general xclip nano-modeline company markdown-mode dune-format darkroom doom-themes racket-mode hide-mode-line utop which-key window-numbering treemacs-all-the-icons dune ocamlformat fsharp-mode magit toml beacon goto-line-preview youdao-dictionary powerline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
