(use-package tuareg
  :defer t
  :commands (ocamlformat-before-save)
  :config
  (use-package ocamlformat)
  (use-package dune-format)
  (use-package dune)
  (require 'opam-user-setup "~/.config/emacs/opam-user-setup.el")
  (define-key tuareg-mode-map (kbd "C-I") 'ocamlformat-before-save))

(provide 'init-ocaml)
