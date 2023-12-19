;; init-fsharp.el --- Initialize fsharp configurations.	-*- lexical-binding: t -*-

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
;; FSharp configuration.
;;

;;; Code:

(use-package fsharp-mode
  :config
  (use-package ob-fsharp)

  (defun fsharp-fantomas-format-region (start end)
    (interactive "r")
    (let ((source (shell-quote-argument (buffer-substring-no-properties start end)))
          (ok-buffer "*fantomas*")
          (error-buffer "*fantomas-errors*"))
      (save-window-excursion
        (shell-command-on-region
         start end (format "fantomas --indent 2 --pageWidth 99 --stdin %s --stdout" source)
         ok-buffer nil error-buffer)
        (if (get-buffer error-buffer)
            (progn
              (kill-buffer error-buffer)
              (message "Can't format region."))
          (delete-region start end)
          (insert (with-current-buffer ok-buffer
                    (s-chomp (buffer-string))))
          (delete-trailing-whitespace)
          (message "Region formatted.")))))

  (defun fsharp-fantomas-format-defun ()
    (interactive)
    (let ((origin (point))
          (start) (end))
      (fsharp-beginning-of-block)
      (setq start (point))
      (fsharp-end-of-block)
      ;; skip whitespace, empty lines, comments
      (while (and (not (= (line-number-at-pos) 1))
                  (s-matches? "^\n$\\|^//\\|^(\\*" (thing-at-point 'line)))
        (forward-line -1))
      (move-end-of-line 1)
      (setq end (point))
      (fsharp-fantomas-format-region start end)
      (goto-char origin)))

  (defun fsharp-fantomas-format-buffer ()
    (interactive)
    (let ((origin (point)))
      (fsharp-fantomas-format-region (point-min) (point-max))
      (goto-char origin)))

  (defun fsharp-load-file (file-name)
    (interactive (comint-get-source "Load F# file: " nil '(fsharp-mode) t))
    (let ((command (concat "#load \"" file-name "\"")))
      (comint-check-source file-name)
      (fsharp-simple-send inferior-fsharp-buffer-name command)))

  (defun fsharp-add-this-file-to-proj ()
    (interactive)
    (when-let* ((file-long (f-this-file))
                (project (fsharp-mode/find-fsproj file-long))
                (file (f-filename file-long)))
      (with-current-buffer (find-file-noselect project)
        (goto-char (point-min))
        (unless (re-search-forward file nil t)
          (when (and (re-search-forward "<Compile Include=" nil t)
                     (re-search-backward "<" nil t))
            (insert (format "<Compile Include=\"%s\" />\n    " file))
            (save-buffer))))))

  (defun fsharp-remove-this-file-from-proj ()
    (interactive)
    (when-let* ((file-long (f-this-file))
                (project (fsharp-mode/find-fsproj file-long))
                (file (f-filename file-long)))
      (with-current-buffer (find-file-noselect project)
        (goto-char (point-min))
        (when (re-search-forward (format "<Compile Include=\"%s\" />" file) nil t)
          (move-beginning-of-line 1)
          (kill-line)
          (kill-line)
          (save-buffer)))))

  (defun fsharp-compile-project ()
    "Compile project using fake or dotnet."
    (interactive)
    (let ((fake-dir (locate-dominating-file default-directory "build.fsx"))
          (proj (fsharp-mode/find-fsproj (or (f-this-file) ""))))
      (cond (fake-dir (let ((default-directory fake-dir)
                            (compile-command "fake build"))
                        (call-interactively 'compile)))
            (proj (let ((compile-command (format "dotnet build \"%s\"" proj)))
                    (call-interactively 'compile)))
            (t (call-interactively 'compile)))))

  (defun fsharp-enable-prettify-symbols ()
    (let ((alist '(("->" . ?→)
                   ("<-" . ?←)
                   ("|>" . ?⊳)
                   ("<|" . ?⊲))))
      (setq-local prettify-symbols-alist alist))))

(provide 'init-fsharp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fsharp.el ends here
