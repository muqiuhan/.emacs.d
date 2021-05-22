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
    (load "~/.emacs.d/config/develop/nox/nox.el")
    (use-package nox)
    (setq nox-completion (list
			  'c-mode-common-hook
			  'c-mode-hook
			  'c++-mode-hook))
    (dolist (hook nox-completion)
      (add-hook hook '(lambda () (nox-ensure))))))
 ((= 2 *lang-c/c++-completion*)
  (progn
    (use-package irony
      :hook
      ((c++-mode . irony-mode)
       (c-mode . irony-mode)))
    (use-package irony-eldoc
      :hook
      (irony-mode . irony-eldoc))))
 ((= 3 *lang-c/c++-completion*)
  (lsp)))

(when *lang-c/c++-flycheck*
  (use-package flycheck
    :hook
    (c++-mode . flycheck-mode)
    (c-mode . flycheck-mode)))

;; 一键编译运行文件，C/C++
(defun process-c/c++-single-file (language)
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
	    " -Wall " source-code-file-name " -o " target-file-name))
	  (insert compile-command))
      (insert "\n*================ 正在编译 ================*\n")
      (insert compile-command)
      (insert "\n*================ 编译结果 ================*\n")
      (let ((compile-result (shell-command-to-string compile-command)))
	(if (= 0 (length compile-result))
	    (insert "编译完成，无错误！")
	  (insert compile-result))))
    
    (insert "\n*================ 运行结果 ================*\n")
    (let ((run-command target-file-name))
      (let ((run-result (shell-command-to-string run-command)))
	(if (= 0 (length run-result))
	    (insert "无输出！")
	  (insert run-result))))))

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
