(require 'package)
(require 'cl-lib)

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
	("http" . "127.0.0.1:7890")
	("https" . "127.0.0.1:7890")))

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
	(if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(require-package 'use-package)
(require-package 'magit)
(require-package 'prescient)
(require-package 'treemacs-persp)
(require-package 'treemacs-projectile)
(require-package 'treemacs-magit)
(require-package 'treemacs-all-the-icons)
(require-package 'doom-themes)
(require-package 'window-numbering)
(require-package 'rainbow-delimiters)
(require-package 'racket-mode)
(require-package 'merlin)
(require-package 'ocamlformat)
(require-package 'dune)
(require-package 'ocp-indent)
(require-package 'tuareg)
(require-package 'yasnippet)
(require-package 'hide-mode-line)
(require-package 'markdown-mode)
(require-package 'nano-modeline)
(require-package 'rustic)
(require-package 'company)
(require-package 'prescient)
(require-package 'company-prescient)
(require-package 'company-box)

(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and t
       (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
           (require 'all-the-icons nil t))))

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind (("M-/"       . company-complete)
         ("C-M-i"     . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("<backtab>" . my-company-yasnippet))
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))
  :config
  (with-no-warnings
    ;; Company anywhere
    ;; @see https://github.com/zk-phi/company-anywhere
    (defun company-anywhere-after-finish (completion)
      (when (and (stringp completion)
                 (looking-at "\\(?:\\sw\\|\\s_\\)+")
                 (save-match-data
                   (string-match (regexp-quote (match-string 0)) completion)))
        (delete-region (match-beginning 0) (match-end 0))))
    (add-hook 'company-after-completion-hook 'company-anywhere-after-finish)

    (defun company-anywhere-grab-word (_)
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
    (advice-add 'company-grab-word :around 'company-anywhere-grab-word)

    (defun company-anywhere-grab-symbol (_)
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))
    (advice-add 'company-grab-symbol :around 'company-anywhere-grab-symbol)

    (defun company-anywhere-dabbrev-prefix (_)
      (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)" company-dabbrev-char-regexp) 1))
    (advice-add 'company-dabbrev--prefix :around 'company-anywhere-dabbrev-prefix)

    (defun company-anywhere-capf (fn command &rest args)
      (if (eq command 'prefix)
          (let ((res (company--capf-data)))
            (when res
              (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
                    (prefix (buffer-substring-no-properties (nth 1 res) (point))))
                (cond
                 (length (cons prefix length))
                 (t prefix)))))
        (apply fn command args)))
    (advice-add 'company-capf :around 'company-anywhere-capf)

    (defun company-anywhere-preview-show-at-point (pos completion)
      (when (and (save-excursion
                   (goto-char pos)
                   (looking-at "\\(?:\\sw\\|\\s_\\)+"))
                 (save-match-data
                   (string-match (regexp-quote (match-string 0)) completion)))
        (move-overlay company-preview-overlay (overlay-start company-preview-overlay) (match-end 0))
        (let ((after-string (overlay-get company-preview-overlay 'after-string)))
          (when after-string
            (overlay-put company-preview-overlay 'display after-string)
            (overlay-put company-preview-overlay 'after-string nil)))))
    (advice-add 'company-preview-show-at-point :after 'company-anywhere-preview-show-at-point)

    ;; `yasnippet' integration
    (with-eval-after-load 'yasnippet
      (defun my-company-yasnippet ()
        "Hide the current completeions and show snippets."
        (interactive)
        (company-cancel)
        (call-interactively 'company-yasnippet))

      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq cmd  'prefix)
            (when-let ((prefix (funcall fn 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fn cmd arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))

  ;; Better sorting
  (use-package prescient
    :commands prescient-persist-mode
    :init (prescient-persist-mode 1))
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Icons and quickhelp
  (use-package company-box
    :diminish
    :bind (:map company-active-map
		([remap company-show-doc-buffer] . company-box-doc-manually))
    :hook (company-mode . company-box-mode)
    :init (setq company-box-enable-icon t
                company-box-backends-colors nil
                company-box-doc-delay 0.1)
    :config
    (with-no-warnings
      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

      ;; Display borders and optimize performance
      (defun my-company-box--display (string on-update)
        "Display the completions."
        (company-box--render-buffer string on-update)

        (let ((frame (company-box--get-frame))
              (border-color (face-foreground 'font-lock-comment-face nil t)))
          (unless frame
            (setq frame (company-box--make-frame))
            (company-box--set-frame frame))
          (company-box--compute-frame-position frame)
          (company-box--move-selection t)
          (company-box--update-frame-position frame)
          (unless (frame-visible-p frame)
            (make-frame-visible frame))
          (company-box--update-scrollbar frame t)
          (set-face-background 'internal-border border-color frame)
          (when (facep 'child-frame-border)
            (set-face-background 'child-frame-border border-color frame)))
        (with-current-buffer (company-box--get-buffer)
          (company-box--maybe-move-number (or company-box--last-start 1))))
      (advice-add #'company-box--display :override #'my-company-box--display)

      (setq company-box-doc-frame-parameters '((vertical-scroll-bars . nil)
                                               (horizontal-scroll-bars . nil)
                                               (internal-border-width . 1)
                                               (left-fringe . 8)
                                               (right-fringe . 8)))

      (defun my-company-box-doc--make-buffer (object)
        (let* ((buffer-list-update-hook nil)
               (inhibit-modification-hooks t)
               (string (cond ((stringp object) object)
                             ((bufferp object) (with-current-buffer object (buffer-string))))))
          (when (and string (length> (string-trim string) 0))
            (with-current-buffer (company-box--get-buffer "doc")
              (erase-buffer)
              (insert (propertize "\n" 'face '(:height 0.5)))
              (insert string)
              (insert (propertize "\n\n" 'face '(:height 0.5)))

              ;; Handle hr lines of markdown
              ;; @see `lsp-ui-doc--handle-hr-lines'
              (let (bolp next before after)
                (goto-char 1)
                (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                  (when (get-text-property next 'markdown-hr)
                    (goto-char next)
                    (setq bolp (bolp)
                          before (char-before))
                    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                    (setq after (char-after (1+ (point))))
                    (insert
                     (concat
                      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                      (propertize "\n" 'face '(:height 0.5))
                      (propertize " "
                                  'display '(space :height (1))
                                  'company-box-doc--replace-hr t
                                  'face `(:background ,(face-foreground 'font-lock-comment-face)))
                      (propertize " " 'display '(space :height (1)))
                      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5))))))))

              (setq mode-line-format nil
                    display-line-numbers nil
                    header-line-format nil
                    show-trailing-whitespace nil
                    cursor-in-non-selected-windows nil)
              (current-buffer)))))
      (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

      ;; Display the border and fix the markdown header properties
      (defun my-company-box-doc--show (selection frame)
        (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                  (window-configuration-change-hook nil)
                  (inhibit-redisplay t)
                  (display-buffer-alist nil)
                  (buffer-list-update-hook nil))
          (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                         company-box--bottom
                                         company-selection
                                         (company-box--get-frame)
                                         (frame-visible-p (company-box--get-frame))))
                       (candidate (nth selection company-candidates))
                       (doc (or (company-call-backend 'quickhelp-string candidate)
                                (company-box-doc--fetch-doc-buffer candidate)))
                       (doc (company-box-doc--make-buffer doc)))
            (let ((frame (frame-local-getq company-box-doc-frame))
                  (border-color (face-foreground 'font-lock-comment-face nil t)))
              (unless (frame-live-p frame)
                (setq frame (company-box-doc--make-frame doc))
                (frame-local-setq company-box-doc-frame frame))
              (set-face-background 'internal-border border-color frame)
              (when (facep 'child-frame-border)
                (set-face-background 'child-frame-border border-color frame))
              (company-box-doc--set-frame-position frame)

              ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
              (with-current-buffer (company-box--get-buffer "doc")
                (let (next)
                  (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                    (when (get-text-property next 'company-box-doc--replace-hr)
                      (put-text-property next (1+ next) 'display
                                         '(space :align-to (- right-fringe 1) :height (1)))
                      (put-text-property (1+ next) (+ next 2) 'display
                                         '(space :align-to right-fringe :height (1)))))))

              (unless (frame-visible-p frame)
                (make-frame-visible frame))))))
      (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

      (defun my-company-box-doc--set-frame-position (frame)
        (-let* ((frame-resize-pixelwise t)

                (box-frame (company-box--get-frame))
                (box-position (frame-position box-frame))
                (box-width (frame-pixel-width box-frame))
                (box-height (frame-pixel-height box-frame))
                ;; (box-border-width (frame-border-width box-frame))

                (window (frame-root-window frame))
                ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                    (/ (frame-pixel-width) 2)
                                                                    (/ (frame-pixel-height) 2)))
                (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

                (x (- (+ (car box-position) box-width) border-width))
                (space-right (- (frame-pixel-width) x))
                (space-left (car box-position))
                (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
                (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
                (width (+ text-width border-width fringe-left fringe-right))
                (x (if (> width space-right)
                       (if (> space-left width)
                           (- space-left width)
                         space-left)
                     x))
                (y (cdr box-position))
                (bottom (+ company-box--bottom (frame-border-width)))
                (height (+ text-height (* 2 border-width)))
                (y (cond ((= x space-left)
                          (if (> (+ y box-height height) bottom)
                              (+ (- y height) border-width)
                            (- (+ y box-height) border-width)))
                         ((> (+ y height) bottom)
                          (- (+ y box-height) height))
                         (t y))))
          (set-frame-position frame (max x 0) (max y 0))
          (set-frame-size frame text-width text-height t)))
      (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

      (when (icon-displayable-p)
        (setq company-box-icons-all-the-icons
              `((Unknown       . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
                (Text          . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
                (Method        . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Function      . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Constructor   . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Field         . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable      . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class         . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface     . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module        . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property      . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
                (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
                (Value         . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum          . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword       . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
                (Snippet       . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
                (Color         . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
                (File          . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
                (Reference     . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
                (Folder        . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
                (EnumMember    . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
                (Constant      . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
                (Struct        . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event         . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator      . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
                (Template      . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
              company-box-icons-alist 'company-box-icons-all-the-icons)))))


(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:background ,(face-foreground 'font-lock-comment-face nil t)))))
  :bind (([f8]        . treemacs)
         ("M-1"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   35
        treemacs-no-png-images           t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-projectile
    :after projectile
    :bind (:map projectile-command-map
		("h" . treemacs-projectile)))

  (use-package treemacs-magit
    :after magit
    :commands treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-persp
    :after persp-mode
    :demand t
    :functions treemacs-set-scope-type
    :config (treemacs-set-scope-type 'Perspectives))

  ;; (use-package treemacs-all-the-icons      ;;
  ;;   :init				      ;;
  ;;   (require 'treemacs-all-the-icons)      ;;
  ;;   (treemacs-load-theme "all-the-icons")) ;;
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package racket-mode
  :hook (racket-mode . racket-xp-mode))

(use-package tuareg
  :config
  (use-package merlin
    :hook (tuareg-mode . merlin-mode))

  (use-package ocamlformat
    :hook (tuareg-mode . ocamlformat-before-save))

  (use-package ocp-indent
    :hook (tuareg-mode . ocp-indent-mode)))

(use-package nano-modeline
  :init
  (nano-modeline-mode))

(use-package hide-mode-line
  :hook (after-init . global-hide-mode-line-mode))

(use-package window-numbering
  :hook (after-init . window-numbering-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t)
  :config
  (set-face-attribute 'line-number nil
		      :background "#009"
		      :font (face-attribute 'default :font)
		      :height (face-attribute 'default :height)
		      :weight (face-attribute 'default :weight))
  
  (set-face-attribute 'line-number-current-line nil
		      :background "#00f"
		      :foreground "#fff"
		      :font (face-attribute 'default :font)
		      :height (face-attribute 'default :height)
		      :weight (face-attribute 'default :weight)))

(defun splash-screen ()
  "Emacs splash screen"
  
  (interactive)
  (let* ((splash-buffer  (get-buffer-create "*splash*"))
         (recover-session (and auto-save-list-file-prefix
                               (file-directory-p (file-name-directory
                                                  auto-save-list-file-prefix))))
         (height         (- (window-body-height nil) 1))
         (width          (window-body-width nil))
         (padding-center (- (/ height 2) 1))
         (padding-bottom (- height (/ height 2) 3)))

    ;; If there are buffer associated with filenames,
    ;;  we don't show splash screen.
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                               if (buffer-file-name buf)
                               collect (buffer-file-name buf))))
        
        (with-current-buffer splash-buffer
          (erase-buffer)
          
          ;; Buffer local settings
          (if (one-window-p)
              (setq mode-line-format nil))
          (setq cursor-type nil)
          (setq vertical-scroll-bar nil)
          (setq horizontal-scroll-bar nil)
          (setq fill-column width)
          (face-remap-add-relative 'link :underline nil)

          ;; Vertical padding to center
          (insert-char ?\n padding-center)

          ;; Central text
          (insert-text-button " www.gnu.org "
			      'action (lambda (_) (browse-url "https://www.gnu.org"))
			      'help-echo "Visit www.gnu.org website"
			      'follow-link t)
          (center-line) (insert "\n")
          (insert (concat
                   (propertize "GNU Emacs"  'face 'bold)
                   " " "version "
                   (format "%d.%d" emacs-major-version emacs-minor-version)))
          (center-line) (insert "\n")
          (insert (propertize "A free/libre editor" 'face 'shadow))
          (center-line)


          ;; Vertical padding to bottom
          (insert-char ?\n padding-bottom)

          ;; Recover session button
          (when recover-session
            (delete-char -2)
            (insert-text-button " [Recover session] "
				'action (lambda (_) (call-interactively 'recover-session))
				'help-echo "Recover previous session"
				'face 'warning
				'follow-link t)
            (center-line) (insert "\n") (insert "\n"))

          ;; Copyright text
          (insert (propertize
                   "GNU Emacs comes with ABSOLUTELY NO WARRANTY" 'face 'shadow))
          (center-line) (insert "\n")
          (insert (propertize
                   "Copyright (C) 2020 Free Software Foundation, Inc." 'face 'shadow))
          (center-line) (insert "\n")

          (goto-char 0)
          (read-only-mode t)
          
          (local-set-key [t]               'splash-screen-fade-to-about)
          (local-set-key (kbd "C-[")       'splash-screen-fade-to-default)
          (local-set-key (kbd "<escape>")  'splash-screen-fade-to-default)
          (local-set-key (kbd "q")         'splash-screen-fade-to-default)
          (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
          (local-set-key (kbd "<mouse-2>") 'operate-this-button)
          (display-buffer-same-window splash-buffer nil)
          (run-with-idle-timer 10.0 nil    'splash-screen-fade-to-about)))))


;; Mac animation, only available from
;;  https://bitbucket.org/mituharu/emacs-mac/src/master/
;;  https://github.com/railwaycat/homebrew-emacsmacport
(defvar mac-animation-locked-p nil)
(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))
(defun mac-animation-fade-out (duration &rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil :type 'fade-out :duration duration)
    (run-with-timer duration nil 'mac-animation-toggle-lock)))

(defun splash-screen-fade-to (about duration)
  "Fade out current frame for duration and goes to command-or-bufffer"
  (interactive)
  (defalias 'mac-animation-fade-out-local
    (apply-partially 'mac-animation-fade-out duration))
  (if (get-buffer "*splash*")
      (progn (if (and (display-graphic-p) (fboundp 'mac-start-animation))
                 (advice-add 'set-window-buffer
                             :before 'mac-animation-fade-out-local))
             (if about (about-emacs))
             (kill-buffer "*splash*")
             (if (and (display-graphic-p) (fboundp 'mac-start-animation))
                 (advice-remove 'set-window-buffer
                                'mac-animation-fade-out-local)))))
(defun splash-screen-fade-to-about ()
  (interactive) (splash-screen-fade-to 1 1.0))
(defun splash-screen-fade-to-default ()
  (interactive) (splash-screen-fade-to nil 0.25))

(defun splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (if (get-buffer "*splash*")
      (kill-buffer "*splash*")))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "-no-splash"  command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args))
         (not inhibit-startup-screen)
         )
    (progn
      (add-hook 'window-setup-hook 'splash-screen)
      (setq inhibit-startup-screen t 
            inhibit-startup-message t
            inhibit-startup-echo-area-message t)))

(fringe-mode 0)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)

(set-face-attribute 'default nil
		    :background "#111"
		    :font "SF Mono"
		    :height 125
		    :weight 'Semibold)

(load-theme 'manoj-dark)
