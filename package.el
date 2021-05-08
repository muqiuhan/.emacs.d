;; 软件包源设置
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; 软件包列表
(defconst *package-list* (list
			  'youdao-dictionary
			  'treemacs
			  'which-key
			  'use-package
			  'doom-modeline
			  'doom-themes
			  'dashboard
			  'highlight-indent-guides
			  'highlight-parentheses
			  'rainbow-delimiters
			  'vterm
			  'window-numbering
			  'posframe
			  'pomidor
			  'company))


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
