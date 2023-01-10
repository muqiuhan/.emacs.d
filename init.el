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
(package-install 'w3m)
(package-install 'dune)
(package-install 'ocamlformat)
(package-install 'ocp-indent)
(package-install 'magit)
(package-install 'rust-mode)
(package-install 'toml)
(package-install 'cargo)
(package-install 'cargo-mode)
(package-install 'racer)
(package-install 'beacon)
(package-install 'goto-line-preview)

;; ----------------------------------- Basic config -----------------------------------
(menu-bar-mode -1) ;; close menubar
(global-auto-revert-mode 1) ;; auto revert/refresh file when change detected
(load-theme 'modus-vivendi t) ;; themes
(set-face-attribute 'default nil :foreground "#eee") ;; make the text less dazzling
(setq backup-directory-alist `(("." . "~/.saves"))) ;; set the unified storage path for backup files

;; GUI
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (fringe-mode 0)

  (set-face-attribute 'default nil
		      :font "CPMono_v07 Bold 12"))

;; company
(require 'company)
(global-company-mode t)

;; line number
(require 'display-line-numbers)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(set-face-attribute 'line-number-current-line nil :background "#00f" :foreground "#ccc")
(set-face-attribute 'line-number nil :background "#005" :foreground "#ccc")

;; xclip: easy to synchorize with the system clipboard
(require 'xclip)
(xclip-mode)

;; treemacs
(require 'treemacs)
(setq treemacs-width 35
      treemacs-indentation 2
      treemacs-icon-tag-leaf "0")

(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "M-0") 'treemacs-select-window)

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
(require 'ocp-indent)
(require 'merlin)
(require 'ocamlformat)

(setq auto-mode-alist (append '(("\\.ml[ily]?$" . tuareg-mode)) auto-mode-alist)
      merlin-command "~/.opam/5.0.0/bin/ocamlmerlin")

(add-hook 'tuareg-mode-hook 'merlin-mode)

;; Quickly format
(define-key tuareg-mode-map (kbd "C-x x f") 'ocamlformat)

;; Rust
(require 'rust-mode)
(require 'racer)
(setq racer-cmd "~/Workspace/racer/target/release/racer")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

;; ----------------------------------- Utils config -----------------------------------

;; Markdown
(require 'markdown-mode)
(require 'writeroom-mode)

(add-hook 'markdown-mode-hook 'writeroom-mode)
(setq writeroom-width (floor (/ (window-width) 1.5)))

;; Add README support
(setq auto-mode-alist (append '(("README" . writeroom-mode)) auto-mode-alist))

;; Goto line preview
(require 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)

;; beacon : easy to visually locate the cursor quickly
(require 'beacon)
(beacon-mode)

;; Mail
;; Personal Information
(setq user-full-name "Muqiu Han"
      user-mail-address "muqiu-han@outlook.com")

;; Send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "muqiu")


(require 'nnir)

;; Please note mail folders in `gnus-select-method' have NO prefix like "nnimap+hotmail:" or "nnimap+gmail:"
(setq gnus-select-method '(nnimap "Outlook"
				  (nnimap-address "outlook.office365.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)
				  (nnir-search-engine imap)
				  (nnmail-expiry-wait 90))) 

;; ask encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

(setq gnus-use-cache t)

;; {{ press "o" to view all groups
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key gnus-group-mode-map
	    ;; list all the subscribed groups even they contain zero un-read messages
	    (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; open attachment
(eval-after-load 'mailcap
  '(progn
     (cond
      ;; on macOS, maybe change mailcap-mime-data?
      ((eq system-type 'darwin))
      ;; on Windows, maybe change mailcap-mime-data?
      ((eq system-type 'windows-nt))
      (t
       ;; Linux, read ~/.mailcap
       (mailcap-parse-mailcaps)))))

;; Tree view for groups.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; Read HTML mail:
(setq mm-text-html-renderer 'w3m)

;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
(setq gnus-use-correct-string-widths nil)

;; Sample on how to organize mail folders.
;; It's dependent on `gnus-topic-mode'.
(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive")
                                (nnfolder-active-file "~/Mail/archive/active")
                                (nnfolder-get-new-mail nil)
                                (nnfolder-inhibit-expiry t))))

     ;; "Gnus" is the root folder
     (setq gnus-topic-topology '(("Gnus" visible)))
     
     ;; each topic corresponds to a public imap folder
     (setq gnus-topic-alist '(("Gnus")))))

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
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(rust-mode merlin markdown-mode treemacs xclip nano-modeline company writeroom-mode racket-mode hide-mode-line tuareg merlin-eldoc dune ocamlformat ocp-indent magit toml cargo cargo-mode racer beacon goto-line-preview w3m centered-window perfect-margin olivetti)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
