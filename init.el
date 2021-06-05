(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/config/config.el")


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helm-xref lsp-treemacs auto-complete-clang auto-complete rtags youdao-dictionary yasnippet window-numbering which-key vterm use-package treemacs-projectile sly-repl-ansi-color sly-quicklisp rustic rust-playground rust-mode rust-auto-use rainbow-delimiters racket-mode pomidor pdf-tools org-bullets netease-music makefile-executor irony-eldoc highlight-parentheses highlight-indent-guides flycheck fira-code-mode evil doom-themes doom-modeline dashboard company ccls cargo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
