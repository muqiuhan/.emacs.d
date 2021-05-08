  (use-package company
    :bind (("M-/" . company-complete)
           ("C-M-i" . company-complete)
           :map company-mode-map
           ("<backtab>" . company-yasnippet)
           :map company-active-map
           ("C-p" . company-select-previous)
           ("C-n" . company-select-next)
           ("<tab>" . company-complete-common-or-cycle)
           ("<backtab>" . my-company-yasnippet)
           :map company-search-map
           ("C-p" . company-select-previous)
           ("C-n" . company-select-next))
    :hook
    (after-init . (lambda () (global-company-mode)))
    :init
    (setq company-tooltip-align-annotations t
          company-tooltip-limit 12
          company-idle-delay 0
          company-echo-delay (if (display-graphic-p) nil 0)
          company-minimum-prefix-length 1
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-global-modes '(not erc-mode message-mode help-mode
                                     gud-mode eshell-mode shell-mode)
          company-backends '((company-capf :with company-yasnippet)
                             (company-dabbrev-code company-keywords company-files)
                             company-dabbrev)))

(provide 'init-company)
