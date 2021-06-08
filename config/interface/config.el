;; interface/config.el --- Initialize interface.-*- lexical-binding: t -*-

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

(add-to-list 'load-path "~/.emacs.d/config/interface/")

(require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-font)

(defun init-frame ()
  (when *frame-font*
    (set-frame-font *frame-font*))
  (menu-bar-mode *frame-menu-bar*)
  (tab-bar-mode *frame-tab-bar*)
  (tool-bar-mode *frame-tool-bar*)
  (set-fringe-style 0)
  (scroll-bar-mode *frame-scroll-bar*)
  (when *frame-theme*
    (load-theme *frame-theme* t))
  (display-battery-mode *frame-battery-mode*)
  (display-time-mode *frame-time-mode*)
  
  (when *frame-start*
    (cond ((= *frame-start* 1)
	   ((lambda ()
	      "设置窗体最大化"
	      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
				     '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
	      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
				     '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))))

	  ((= *frame-start* 2)
	   ((lambda ()
	      "设置窗体全屏"
	      (interactive)
	      (set-frame-parameter nil 'fullscreen
				   (if (frame-parameter nil 'fullscreen) nil 'fullboth))))))))

(defun init-editor ()
  (when *editor-line-number*
    (setq linum-format " %d ")
    (global-linum-mode))

  (when *editor-high-line*
    (global-hl-line-mode))

  

  (when *editor-smooth-scrolling*
    (setq mouse-wheel-scroll-amount '(*editor-smooth-scrolling-size* ((shift) . *editor-smooth-scrolling-shift-size*) ((control) . nil)))
    (setq mouse-wheel-progressive-speed nil))

  (when *editor-which-function-mode*
    (which-function-mode))
  
  (cond ((= *editor-modeline-style* 1)
	 (doom-modeline-mode))
	((= *editor-modeline-style* 2)
	 (powerline-default-theme)))

  (when *editor-cursor-type*
    (setq cursor-type *editor-cursor-type*))
  
  (when *editor-cursor-color*
    (set-cursor-color *editor-cursor-color*)))

(init-frame)
(init-editor)
