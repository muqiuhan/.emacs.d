;; init-ocaml.el --- Initialize ocaml configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2023 Muqiu Han

;; Author: Muqiu han <muqiu-han@outlook.com>
;; URL: https://github.com/muqiuhan/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

;;; Commentary:
;;
;; OCaml configuration.
;;

;;; Code:

(use-package tuareg
  :defer t
  :commands (ocamlformat-before-save)
  :config
  (use-package ocamlformat)
  (use-package dune-format)
  (use-package dune)
  (require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
  (define-key tuareg-mode-map (kbd "C-I") 'ocamlformat-before-save))

(provide 'init-ocaml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ocaml.el ends here
