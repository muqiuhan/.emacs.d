(when *start-server*
  (server-start))

(when *start-package-init*
  (load "~/.emacs.d/package.el"))

(load "~/.emacs.d/config/develop/config.el")
(load "~/.emacs.d/config/interface/config.el")
(load "~/.emacs.d/config/other/config.el")
