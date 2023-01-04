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

;; Package config
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(package-install 'treemacs)
(package-install 'xclip)
(package-install 'nano-modeline)
(package-install 'company)
(package-install 'markdown-mode)
(package-install 'writeroom-mode)
(package-install 'racket-mode)
(package-install 'hide-mode-line)
(package-install 'tuareg)
(package-install 'merlin)
(package-install 'merlin-eldoc)
(package-install 'dune)
(package-install 'ocamlformat)
(package-install 'ocp-indent)
(package-install 'magit)

;; Close the menu bar
(menu-bar-mode -1)

;; Company
(require 'company)
(global-company-mode t)

;; Line number
(require 'display-line-numbers)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(set-face-attribute 'line-number-current-line nil :background "#00f" :foreground "#ccc")
(set-face-attribute 'line-number nil :background "#005" :foreground "#ccc")

;; Xclip
(require 'xclip)
(xclip-mode)

;; Treemacs
(require 'treemacs)
(setq treemacs-width 50
      treemacs-indentation 2
      treemacs-icon-tag-leaf "0")

(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "M-0") 'treemacs-select-window)

;; Themes
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :foreground "#eee")

;; Modeline
(require 'hide-mode-line)
(require 'nano-modeline)
(global-hide-mode-line-mode t)
(nano-modeline)

;; Set the unified storage path for backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Racket
(require 'racket-mode)
(add-hook 'racket-mode racket-xp-mode)

;; OCaml
;; tuareg-mode
(require 'tuareg)
(setq auto-mode-alist (append '(("\\.ml[ily]?$" . tuareg-mode)) auto-mode-alist))

;; ocp-indent
(require 'ocp-indent)

;; merlin
(require 'merlin)
(setq merlin-command "~/.opam/5.0.0/bin/ocamlmerlin")
(add-hook 'tuareg-mode-hook 'merlin-mode)

;; Markdown
(require 'markdown-mode)
(require 'writeroom-mode)
(add-hook 'markdown-mode-hook 'writeroom-mode)
(setq writeroom-width (floor (/ (window-width) 1.5)))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-offsets-alist
   '((defun-open . 2)
     (defun-close . 0)
     (class-open . 2)
     (class-close . 2)
     (access-label . -1)))
 '(package-selected-packages
   '(magit markdown-mode merlin treemacs xclip nano-modeline company sweet-theme hide-mode-line racket-mode tuareg merlin-eldoc dune ocamlformat ocp-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
