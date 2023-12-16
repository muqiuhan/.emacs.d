(use-package scala-mode
  :defer t
  :interpreter ("scala" . scala-mode)
  :config
  (use-package sbt-mode
    :commands sbt-start sbt-command
    :config
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    (setq sbt:program-options '("-Dsbt.supershell=false"))))

(provide 'init-scala)
