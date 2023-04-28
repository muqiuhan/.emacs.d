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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
	("http" . "127.0.0.1:7890")
	("https" . "127.0.0.1:7890")))

(defun require-package (&rest packages)
  (dolist (p packages)
    (unless (package-installed-p p)
      (condition-case nil (package-install p)
        (error
         (package-refresh-contents)
         (package-install p))))))

(require-package 'treemacs
		 'xclip
		 'nano-modeline
		 'company
		 'markdown-mode
		 'dune-format
		 'darkroom
		 'racket-mode
		 'hide-mode-line
		 'utop
		 'which-key
		 'tuareg
		 'window-numbering
		 'treemacs-all-the-icons
		 'dune
		 'lsp-mode
		 'ocamlformat
		 'fsharp-mode
		 'magit
		 'toml
		 'cargo
		 'cargo-mode
		 'beacon
		 'goto-line-preview
		 'rustic
		 'youdao-dictionary)

;; ----------------------------------- Basic config -----------------------------------
(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (fringe-mode -1)
  (scroll-bar-mode -1)

  (set-face-attribute 'default nil
		      :font "Victor Mono"
		      :weight 'bold
		      :height 120))

(setq-default line-spacing 0.2)

(global-auto-revert-mode 1) ;; auto revert/refresh file when change detected
(setq backup-directory-alist `(("." . "~/.saves"))) ;; set the unified storage path for backup files

;; company
(require 'company)
(global-company-mode t)

;; line number
(require 'display-line-numbers)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; xclip: easy to synchorize with the system clipboard
(require 'xclip)
(xclip-mode)

;; treemacs
(require 'treemacs)

(setq treemacs-width 50
      treemacs-indentation 2
      treemacs-position 'right
      treemacs-icon-tag-leaf "0")

(when (display-graphic-p)
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "M-RET") 'treemacs-select-window)

;; modeline
(require 'hide-mode-line)
(require 'nano-modeline)

(add-hook 'after-init-hook 'global-hide-mode-line-mode)
(add-hook 'after-init-hook 'nano-modeline-mode)

;; ----------------------------------- Develop config -----------------------------------

;; Lsp
(require 'lsp)
(setq-default lsp-headerline-breadcrumb-enable nil)

;; Racket
(require 'racket-mode)
(add-hook 'racket-mode racket-xp-mode)

;; OCaml
(require 'tuareg)
(require 'ocamlformat)

(add-hook 'tuareg-mode-hook 'lsp)
(define-key tuareg-mode-map (kbd "C-x x f") 'ocamlformat)

;; ----------------------------------- Utils config -----------------------------------

;; window numbering
(require 'window-numbering)
(add-hook 'after-init-hook 'window-numbering-mode)

;; Markdown
(require 'markdown-mode)
(require 'darkroom)

(add-hook 'markdown-mode-hook 'darkroom-mode)
(setq darkroom-margin-increment 20)

;; Add README support
(setq auto-mode-alist (append '(("README" . darkroom-mode)) auto-mode-alist))

;; Goto line preview
(require 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)

;; beacon : easy to visually locate the cursor quickly
(require 'beacon)
(beacon-mode)

;; Translate
(require 'youdao-dictionary)

(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+)
(global-set-key (kbd "C-c p") 'youdao-dictionary-play-voice-at-point)

;; Which key
(require 'which-key)
(which-key-mode t)

;; Theme
(load-theme 'manoj-dark t)

(set-face-attribute 'line-number 'nil :foreground "#090")
(set-face-attribute 'line-number-current-line 'nil :foreground "#0f0")

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el ends here
