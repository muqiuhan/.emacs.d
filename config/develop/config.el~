;; develop/config.el --- Initialize develop.-*- lexical-binding: t -*-

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

(add-to-list 'load-path "~/.emacs.d/config/develop/")

(require 'init-company)
(require 'init-org)
(require 'init-rust)
(require 'init-rtags)

(when *lang-enable-racket*
  (require 'init-racket))

(when *lang-enable-c/c++*
  (require 'init-c))
