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

(setq-default org-enviroment t)
(setq-default org-file-directory "~/Documents")
(setq-default ocaml-environment t)
(setq-default racket-environment t)
(setq-default clojure-environment t)
(setq-default agda-environment t)
(setq-default coq-environment t)
(setq-default backup-directory-alist `(("." . "~/.saves")))
(setq-default gc-cons-threshold (* 50 1000 1000))
(setq-default line-spacing 0.7)
(setq-default cursor-type 'hbar)
(setq-default font "Vin Mono Pro")
(setq-default font-weight 'extrabold)
(setq-default font-size 17)
(setq-default chinese-font "TsangerMingHei")
(setq-default chinese-font-weight 'bold)
(setq-default chinese-font-size 17)
(setq-default theme 'doom-gruvbox)
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
		         'company
		         'company-prescient
		         'markdown-mode
		         'flymake-popon
		         'nano-modeline
		         'eglot
		         'vterm
		         'vterm-toggle
		         'which-key
		         'hide-mode-line
		         'window-numbering
		         'doom-themes
		         'magit
                 'sideline-flymake
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
  '())


;; ----------------------------------- Basic config -----------------------------------

(toggle-frame-maximized)

(global-auto-revert-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(scroll-bar-mode -1)

(load-theme theme t)

;; Save your eyes!!!
(if (string-equal "#000000" (face-attribute 'default :background))
    (set-face-attribute 'default nil :background "#191919"))

(set-default 'truncate-lines t)

(when is-graphics
  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil
			            :font (format "%s:pixelsize=%d" english english-size)
			            :weight font-weight)

    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			            (font-spec
			             :family chinese
			             :size chinese-size
			             :weight chinese-font-weight))))
  (set-font font chinese-font font-size chinese-font-size))

;; ----------------------------------- config -----------------------------------

;; company
(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))

  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))

  :config
  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  (when is-graphics
    (use-package company-box
      :diminish
      :defines company-box-icons-all-the-icons
      :hook (company-mode . company-box-mode)
      :init (setq company-box-enable-icon t
                  company-box-backends-colors nil
                  company-box-doc-delay 0.1)
      :config
      (with-no-warnings
	    (defun my-company-box-icons--elisp (candidate)
          (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
            (let ((sym (intern candidate)))
              (cond ((fboundp sym) 'Function)
                    ((featurep sym) 'Module)
                    ((facep sym) 'Color)
                    ((boundp sym) 'Variable)
                    ((symbolp sym) 'Text)
                    (t . nil)))))
	    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

	    ;; Display borders and optimize performance
	    (defun my-company-box--display (string on-update)
          "Display the completions."
          (company-box--render-buffer string on-update)

          (let ((frame (company-box--get-frame))
		        (border-color (face-foreground 'font-lock-comment-face nil t)))
            (unless frame
              (setq frame (company-box--make-frame))
              (company-box--set-frame frame))
            (company-box--compute-frame-position frame)
            (company-box--move-selection t)
            (company-box--update-frame-position frame)
            (unless (frame-visible-p frame)
              (make-frame-visible frame))
            (company-box--update-scrollbar frame t)
            (set-face-background 'internal-border border-color frame)
            (when (facep 'child-frame-border)
              (set-face-background 'child-frame-border border-color frame)))
          (with-current-buffer (company-box--get-buffer)
            (company-box--maybe-move-number (or company-box--last-start 1))))

	    (advice-add #'company-box--display :override #'my-company-box--display)

	    (setq company-box-doc-frame-parameters '((internal-border-width . 1)
						                         (left-fringe . 8)
						                         (right-fringe . 8)))

	    (defun my-company-box-doc--make-buffer (object)
          (let* ((buffer-list-update-hook nil)
		         (inhibit-modification-hooks t)
		         (string (cond ((stringp object) object)
                               ((bufferp object) (with-current-buffer object (buffer-string))))))
            (when (and string (> (length (string-trim string)) 0))
              (with-current-buffer (company-box--get-buffer "doc")
		        (erase-buffer)
		        (insert (propertize "\n" 'face '(:height 0.5)))
		        (insert string)
		        (insert (propertize "\n\n" 'face '(:height 0.5)))

		        (with-current-buffer (company-box--get-buffer "doc")
                  (let (bolp next before after)
                    (goto-char 1)
                    (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                      (when (get-text-property next 'markdown-hr)
			            (goto-char next)
			            (setq bolp (bolp)
                              before (char-before))
			            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
			            (setq after (char-after (1+ (point))))
			            (insert
			             (concat
                          (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                          (propertize "\n" 'face '(:height 0.5))
                          (propertize " "
                                      'display '(space :height (1))
                                      'company-box-doc--replace-hr t
                                      'face `(:background ,(face-foreground 'font-lock-comment-face)))
                          (propertize " " 'display '(space :height (1)))
                          (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))

		        (setq mode-line-format nil
                      display-line-numbers nil
                      header-line-format nil
                      show-trailing-whitespace nil
                      cursor-in-non-selected-windows nil)
		        (current-buffer)))))
	    (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

	    ;; Display the border and fix the markdown header properties
	    (defun my-company-box-doc--show (selection frame)
          (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                    (window-configuration-change-hook nil)
                    (inhibit-redisplay t)
                    (display-buffer-alist nil)
                    (buffer-list-update-hook nil))
            (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                           company-box--bottom
                                           company-selection
                                           (company-box--get-frame)
                                           (frame-visible-p (company-box--get-frame))))
			             (candidate (nth selection company-candidates))
			             (doc (or (company-call-backend 'quickhelp-string candidate)
                                  (company-box-doc--fetch-doc-buffer candidate)))
			             (doc (company-box-doc--make-buffer doc)))
              (let ((frame (frame-local-getq company-box-doc-frame))
                    (border-color (face-foreground 'font-lock-comment-face nil t)))
		        (unless (frame-live-p frame)
                  (setq frame (company-box-doc--make-frame doc))
                  (frame-local-setq company-box-doc-frame frame))
		        (set-face-background 'internal-border border-color frame)
		        (when (facep 'child-frame-border)
                  (set-face-background 'child-frame-border border-color frame))
		        (company-box-doc--set-frame-position frame)

		        (with-current-buffer (company-box--get-buffer "doc")
                  (let (next)
                    (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                      (when (get-text-property next 'company-box-doc--replace-hr)
			            (put-text-property next (1+ next) 'display
                                           '(space :align-to (- right-fringe 1) :height (1)))
			            (put-text-property (1+ next) (+ next 2) 'display
                                           '(space :align-to right-fringe :height (1)))))))

		        (unless (frame-visible-p frame)
                  (make-frame-visible frame))))))
	    (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

	    (defun my-company-box-doc--set-frame-position (frame)
          (-let* ((frame-resize-pixelwise t)

                  (box-frame (company-box--get-frame))
                  (box-position (frame-position box-frame))
                  (box-width (frame-pixel-width box-frame))
                  (box-height (frame-pixel-height box-frame))
                  (box-border-width (frame-border-width box-frame))

                  (window (frame-root-window frame))
                  ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                      (/ (frame-pixel-width) 2)
                                                                      (/ (frame-pixel-height) 2)))
                  (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

                  (x (- (+ (car box-position) box-width) border-width))
                  (space-right (- (frame-pixel-width) x))
                  (space-left (car box-position))
                  (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
                  (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
                  (width (+ text-width border-width fringe-left fringe-right))
                  (x (if (> width space-right)
			             (if (> space-left width)
                             (- space-left width)
                           space-left)
                       x))
                  (y (cdr box-position))
                  (bottom (+ company-box--bottom (frame-border-width)))
                  (height (+ text-height (* 2 border-width)))
                  (y (cond ((= x space-left)
                            (if (> (+ y box-height height) bottom)
				                (+ (- y height) border-width)
                              (- (+ y box-height) border-width)))
                           ((> (+ y height) bottom)
                            (- (+ y box-height) height))
                           (t y))))
            (set-frame-position frame (max x 0) (max y 0))
            (set-frame-size frame text-width text-height t)))
	    (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
                (Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
                (Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
                (Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
                (Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
                (File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
                (Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
                (Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
              company-box-icons-alist 'company-box-icons-all-the-icons)))))

;; Flymake

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe)
  :config (setq elisp-flymake-byte-compile-load-path
                (append elisp-flymake-byte-compile-load-path load-path))
  (when is-graphics
    (use-package sideline-flymake
      :diminish sideline-mode
      :hook (flymake-mode . sideline-mode)
      :init (setq sideline-flymake-display-mode 'point
                  sideline-backends-right '(sideline-flymake)))))

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
