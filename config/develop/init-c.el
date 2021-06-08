;; init-c.el --- Initialize c-*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Muqiu Han

;; Author: Muqiu Han <muqiu-han@outlook.com>
;; URL: https://github.com/muqiuhan/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Code:

(use-package projectile
  :hook
  (c-mode . projectile-mode)
  (c++-mode . projectile-mode)
  :bind
  ([f5] . 'projectile-find-file)
  :config
  (setq projectile-enable-caching t))

(cond
 ((not *lang-c/c++-completion*) (message "no-completion"))
 ((= 1 *lang-c/c++-completion*)
  (progn
    (require 'auto-complete)
    (require 'auto-complete-config)
    (require 'auto-complete-clang)
    
    (setq ac-auto-start t)
    (setq ac-quick-help-delay 0)
    
    (defun my-ac-config ()  
      (setq ac-clang-flags  
            (mapcar(lambda (item)(concat "-I" item))  
		   (split-string  
                    "/usr/include/c++/10.2.0
 /usr/include/c++/10.2.0/.
 /usr/include/c++/10.2.0/backward
 /usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include
 /usr/local/include
 /usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include-fixed
 /usr/include/gnu/
 /usr/include"
		    )))
      (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))  
      (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)  
      (add-hook 'c-modehook 'ac-cc-mode-setup)
      (add-hook 'c++-mode-hook 'ac-cc-mode-setup)
      (add-hook 'auto-complete-mode-hook 'ac-common-setup)  
      (global-auto-complete-mode t))  
    (defun my-ac-cc-mode-setup ()  
      (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))  
    (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)  
    (my-ac-config)  
    (ac-config-default)))
 
 ((= 2 *lang-c/c++-completion*)
  (progn
    (use-package irony
      :config
      :hook
      ((c++-mode . irony-mode)
       (c-mode . irony-mode)))
    (use-package irony-eldoc
      :hook
      (irony-mode . irony-eldoc))))
 ((= 3 *lang-c/c++-completion*)
  (progn
    (require 'lsp)
    (require 'dap-mode)
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)

    (setq lsp-idle-delay 0)

    (with-eval-after-load 'lsp-mode
      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
      (require 'dap-cpptools)
      (yas-global-mode)))))

(when *lang-c/c++-flycheck*
  (use-package flycheck
    :hook
    (c++-mode . flycheck-mode)
    (c-mode . flycheck-mode)
    )
  )

;; 一键编译运行文件，C/C++
(defun process-c/c++-single-file (language)
  (interactive)
  (let* ((buffer-name (concat "*" (buffer-name) "-compile-and-run*"))
	 (source-code-file-name (buffer-file-name (current-buffer)))
	 (target-file-name (string-remove-suffix
			    (if (string-equal language "c")
				".c" ".cpp")
			    source-code-file-name)))
    (generate-new-buffer buffer-name)
    (split-window-horizontally)
    (switch-to-buffer buffer-name)
    (let ((compile-command
	   (concat
	    (if (string-equal language "c")
		"gcc" "g++")
	    " -Wall " source-code-file-name " -o " target-file-name)))
      (eshell-command compile-command))

    (let ((run-command target-file-name))
      (eshell-command run-command))))

(add-hook 'c-mode-hook '(lambda ()
			  (interactive)
			  (local-set-key [f9] '(lambda ()
						 (interactive)
						 (process-c/c++-single-file "c")))))

(add-hook 'c++-mode-hook '(lambda ()
			    (interactive)
			    (local-set-key [f9] '(lambda ()
						   (interactive)
						   (process-c/c++-single-file "c++")))))


(provide 'init-c)
