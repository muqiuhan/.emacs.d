;; init-high.el --- Initialize highlight.	-*- lexical-binding: t -*-

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

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
  :hook
  (c++-mode . highlight-indent-guides-mode)
  (c-mode . highlight-indent-guides-mode)
  (lisp-mode . highlight-indent-guides-mode)
  (emacs-lisp-mode . highlight-indent-guides-mode))

(use-package highlight-parentheses
  :hook
  (lisp-mode . highlight-parentheses-mode)
  (emacs-lisp-mode . highlight-parentheses-mode))

(use-package rainbow-delimiters
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

(provide 'init-high)
