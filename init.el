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
  (scroll-bar-mode -1))

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

(setq treemacs-width 35
      treemacs-indentation 2
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

;; Mail
(require 'nnir)

(setq user-full-name "Muqiu Han"
      user-mail-address "muqiu-han@outlook.com")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "muqiu")

(setq gnus-select-method '(nnimap "Outlook"
				  (nnimap-address "outlook.office365.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)
				  (nnir-search-engine imap)
				  (nnmail-expiry-wait 90))
      
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date
				   (not gnus-thread-sort-by-number))
      
      gnus-use-cache t
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-thread-hide-subtree nil
      gnus-thread-ignore-subject t
      gnus-use-correct-string-widths nil)

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(define-key gnus-group-mode-map (kbd "o") 'gnus-group-list-all-groups)

(eval-after-load 'mailcap (mailcap-parse-mailcaps))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq mm-text-html-renderer 'w3m)

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y"))
	   gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive")
                                (nnfolder-active-file "~/Mail/archive/active")
                                (nnfolder-get-new-mail nil)
                                (nnfolder-inhibit-expiry t)))
	   gnus-topic-topology '(("Gnus" visible))
	   gnus-topic-alist '(("Gnus")))))

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
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(package-selected-packages
   '(lsp-mode xclip nano-modeline company dune-format darkroom racket-mode hide-mode-line utop which-key treemacs-all-the-icons merlin-eldoc dune ocamlformat ob-fsharp ocp-indent magit toml cargo cargo-mode racer beacon goto-line-preview youdao-dictionary rustic eglot-fsharp window-numbering)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
