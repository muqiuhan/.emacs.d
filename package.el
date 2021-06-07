;; 软件包源设置
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; 软件包列表
(defconst *package-list* (list
			  'sly
			  'sly-quicklisp
			  'sly-repl-ansi-color
			  'youdao-dictionary
			  'org-mind-map
			  'treemacs
			  'evil
			  'ac-rtags
			  'helm
			  'helm-rtags
			  'rtags
			  'auto-complete
			  'lsp-treemacs
			  'helm-lsp
			  'helm-xref
			  'dap-mode
			  'auto-complete-clang
			  'irony
			  'rustic
			  'rust-auto-use
			  'rust-playground
			  'cargo
			  'irony-eldoc
			  'which-key
			  'netease-music
			  'use-package
			  'doom-modeline
			  'doom-themes
			  'dashboard
			  'highlight-indent-guides
			  'rust-mode
			  'highlight-parentheses
			  'rainbow-delimiters
			  'vterm
			  'projectile
			  'makefile-executor
			  'yasnippet
			  'racket-mode
			  'treemacs-projectile
			  'org-bullets
			  'window-numbering
			  'ccls
			  'posframe
			  'pdf-tools
			  'pomidor
			  'company
			  ))


(defun init-package ()
  (defun require-package (package &optional min-version no-refresh)
    (if (package-installed-p package min-version)
	t
      (if (or (assoc package package-archive-contents) no-refresh)
	  (if (boundp 'package-selected-packages)
	      (package-install package nil)
	    (package-install package))
	(progn
	  (package-refresh-contents)
	  (require-package package min-version t)))))
  (while *package-list*
    (require-package (car *package-list*))
    (setq *package-list* (cdr *package-list*))))

(package-initialize)
(init-package)
