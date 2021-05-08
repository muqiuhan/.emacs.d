;; init-company.el --- Initialize company-*- lexical-binding: t -*-

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

(use-package company
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
    :hook
    (after-init . (lambda () (global-company-mode)))
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
                             company-dabbrev)))

(provide 'init-company)
