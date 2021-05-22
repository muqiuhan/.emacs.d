;; init-pdf.el --- Initialize pdf-tool.	-*- lexical-binding: t -*-

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

(use-package pdf-view
  :ensure pdf-tools
  :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
  :commands pdf-view-midnight-minor-mode
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init (setq pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install t nil t nil)

  (defun my-pdf-view-set-midnight-colors ()
    (setq pdf-view-midnight-colors
          `(,(face-foreground 'default) . ,(face-background 'default))))

  (defun my-pdf-view-set-dark-theme ()
    (my-pdf-view-set-midnight-colors)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pdf-view-mode)
          (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))

  (my-pdf-view-set-midnight-colors)
  (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)

  (with-no-warnings
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)

    (defun my-pdf-view-use-scaling-p ()
      "Return t if scaling should be used."
      (and (or (and (eq (framep-on-display) 'ns) (>= emacs-major-version 27))
               (memq (pdf-view-image-type) '(imagemagick image-io)))
           pdf-view-use-scaling))
    (advice-add #'pdf-view-use-scaling-p :override #'my-pdf-view-use-scaling-p)

    (defun my-pdf-view-create-page (page &optional window)
      "Create an image of PAGE for display on WINDOW."
      (let* ((size (pdf-view-desired-image-size page window))
             (width (if (not (pdf-view-use-scaling-p))
                        (car size)
                      (* 2 (car size))))
             (data (pdf-cache-renderpage
                    page width width))
             (hotspots (pdf-view-apply-hotspot-functions
                        window page size)))
        (pdf-view-create-image data
			       :width width
			       :scale (if (pdf-view-use-scaling-p) 0.5 1)
			       :map hotspots
			       :pointer 'arrow)))
    (advice-add #'pdf-view-create-page :override #'my-pdf-view-create-page)

    (defun my-pdf-util-frame-scale-factor ()
      "Return the frame scale factor depending on the image type used for display."
      (if (and pdf-view-use-scaling
               (memq (pdf-view-image-type) '(imagemagick image-io))
               (fboundp 'frame-monitor-attributes))
          (or (cdr (assq 'backing-scale-factor (frame-monitor-attributes)))
              (if (>= (pdf-util-frame-ppi) 180)
                  2
                1))
        (if (and pdf-view-use-scaling (eq (framep-on-display) 'ns))
            2
          1)))
    (advice-add #'pdf-util-frame-scale-factor :override #'my-pdf-util-frame-scale-factor)

    (defun my-pdf-isearch-hl-matches (current matches &optional occur-hack-p)
      "Highlighting edges CURRENT and MATCHES."
      (cl-destructuring-bind (fg1 bg1 fg2 bg2)
          (pdf-isearch-current-colors)
        (let* ((width (car (pdf-view-image-size)))
               (page (pdf-view-current-page))
               (window (selected-window))
               (buffer (current-buffer))
               (tick (cl-incf pdf-isearch--hl-matches-tick))
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (and (null status)
                             (eq tick pdf-isearch--hl-matches-tick)
                             (buffer-live-p buffer)
                             (window-live-p window)
                             (eq (window-buffer window)
                                 buffer))
                    (with-selected-window window
                      (when (and (derived-mode-p 'pdf-view-mode)
                                 (or isearch-mode
                                     occur-hack-p)
                                 (eq page (pdf-view-current-page)))
                        (pdf-view-display-image
                         (pdf-view-create-image data :width width))))))))
          (pdf-info-renderpage-text-regions
           page width t nil
           `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                          current))
           `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                          (apply 'append
                                 (remove current matches))))))))
    (advice-add #'pdf-isearch-hl-matches :override #'my-pdf-isearch-hl-matches)

    (defun pdf-annot-show-annotation (a &optional highlight-p window)
      "Make annotation A visible."
      (save-selected-window
        (when window (select-window window))
        (pdf-util-assert-pdf-window)
        (let ((page (pdf-annot-get a 'page))
              (size (pdf-view-image-size)))
          (unless (= page (pdf-view-current-page))
            (pdf-view-goto-page page))
          (let ((edges (pdf-annot-get-display-edges a)))
            (when highlight-p
              (pdf-view-display-image
               (pdf-view-create-image
                (pdf-cache-renderpage-highlight
                 page (car size)
                 `("white" "steel blue" 0.35 ,@edges))
                :map (pdf-view-apply-hotspot-functions
                      window page size)
                :width (car size))))
            (pdf-util-scroll-to-edges
             (pdf-util-scale-relative-to-pixel (car edges)))))))
    (advice-add #'pdf-annot-show-annotation :override #'my-pdf-annot-show-annotation)))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (centaur-read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  (with-no-warnings
    ;; HACK: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  ;; Fix encoding issue on Windows
  (when sys/win32p
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist))))

(provide 'init-pdf)
