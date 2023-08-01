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

(setq default-frame-alist '((internal-border-width . 5)
			    (vertical-scroll-bars)
			    (left-fringe . 0)
			    (right-fringe . 0)))

(setq-default line-spacing 0
	      cursor-type 'bar)

(set-face-attribute 'default nil
		    :font "IntelOne Mono"
		    :weight 'bold
		    :height 130)

(load-theme 'doom-dark+ t)

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
		 'racket-mode
		 'eglot
		 'merlin-eldoc
		 'vterm
		 'vterm-toggle
		 'utop
		 'which-key
		 'simple-modeline
		 'flycheck-inline
		 'flycheck-ocaml
		 'doom-themes
		 'flycheck
		 'tuareg
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
		 'youdao-dictionary)

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

;; mode line
(use-package simple-modeline
  :defer t
  :hook (after-init . simple-modeline-mode))

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
  :hook (after-init . window-numbering-mode))

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

;; Translate
(use-package youdao-dictionary
  :defer t
  :bind ("C-c p" . youdao-dictionary-play-voice-at-point)
  :init
  (require 'popup)
  (if is-graphics
      (global-set-key (kbd "C-c y")  'youdao-dictionary-search-at-point-posframe)
    (global-set-key (kbd "C-c y")  'youdao-dictionary-search-at-point+)))

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
 '(custom-safe-themes
   '("449a6a03953bf946290f1a0eac09a7de2cf2556aa890f28ad2510a494eb23dcb" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(package-selected-packages
   '(vscdark-theme youdao-dictionary xclip window-numbering which-key vterm-toggle utop treemacs-all-the-icons texfrag simple-modeline rainbow-delimiters racket-mode proof-general olivetti ocamlformat merlin-eldoc markdown-mode makey magit lean-mode goto-line-preview goto-chg flymake-popon flycheck-ocaml flycheck-inline eldoc-box eglot-fsharp dune-format dune doom-themes company-coq company-box cider beacon)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
