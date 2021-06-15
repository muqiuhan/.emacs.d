;; init-font.el --- Initialize font-*- lexical-binding: t -*-

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

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symhttps://github.com/johnw42/fira-code-emacs/tree/217f3f540d8d25fb825da484b076d1e4345e6150bols
instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))

(defun cascadia-ligature-list (ligatures codepoint-start)
  "Create an alist of strings to replace with
codepoints starting from codepoint-start."
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))



(setq cascadia-ligatures
      (let* ((ligs '("x" "www" "*" ":" "-" "--" "---" "-->" "-|" "->"
                     "->>" "--<" "-<<" "-~" "{|" ")#" "[|" "]#" "[|" "]#"
                     "..." "..=" "..<" ".?" ".=" "::" ":::" "::=" ":=" ":>"
                     ":<" ";;" "!!" "!!." "!=" "!==" "?." "?:" "??" "?="
                     "**" "***" "*>" "*/" "#(" "#{" "#[" "#:" "#!" "#?"
                     "##" "###" "####" "#=" "#_" "#_(" "/*" "/=" "/==" "/>"
                     "//" "///" "_|_" "__" "+" "@" "&&" "|-" "|}" "|]"
                     "||" "|||>" "||=" "||>" "|=" "|>" "$>" "++" "+++" "+>"
                     "=:=" "=!=" "==" "===" "==>" "=>" "=>>" "=<<" "=/=" ">-"
                     ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<-" "<--"
                     "<->" "<-<" "<:" "<!--" "<*" "<*>" "<|" "<||" "<|||" "<|>"
                     "<$" "<$>" "<+" "<+>" "<=" "<==" "<==>" "<=>" "<=<" "<>"
                     "<<" "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>" "~-"
                     "~@" "~=" "~>" "~~" "~~>" "^=" "%%")))
	(my-correct-symbol-bounds (cascadia-ligature-list ligs #Xe100))))

;; nice glyphs for haskell with hasklig
(defun set-cascadia-ligatures ()
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (set-fontset-font t '(#Xe100 . #Xe189) "Cascadia Emacs")
  (setq prettify-symbols-alist
        (append cascadia-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))


(add-hook 'after-init-hook #'set-cascadia-ligatures)

(provide 'init-font)
