(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/config/config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1f2430" "#f28779" "#bae67e" "#ffd580" "#73d0ff" "#d4bfff" "#5ccfe6" "#cbccc6"])
 '(custom-safe-themes
   '("0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "5379937b99998e0510bd37ae072c7f57e26da7a11e9fb7bced8b94ccc766c804" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(fci-rule-color "#1c1e23")
 '(jdee-db-active-breakpoint-face-colors (cons "#3d434d" "#ffcc66"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#3d434d" "#bae67e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#3d434d" "#171b24"))
 '(objed-cursor-color "#f28779")
 '(package-selected-packages
   '(sly-repl-ansi-color sly-quicklisp sly cargo rust-playground rust-auto-use rustic rust-mode pretty-mode pretty-hydra yasnippet racket-mode flycheck irony-eldoc irony netease-music treemacs-projectile projectile makefile-executor ccls org-bullets company pomidor window-numbering vterm rainbow-delimiters highlight-parentheses highlight-indent-guides dashboard doom-themes doom-modeline use-package which-key treemacs youdao-dictionary))
 '(pdf-view-incompatible-modes
   '(linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode))
 '(pdf-view-midnight-colors (cons "#cbccc6" "#1f2430"))
 '(rustic-ansi-faces
   ["#1f2430" "#f28779" "#bae67e" "#ffd580" "#73d0ff" "#d4bfff" "#5ccfe6" "#cbccc6"])
 '(safe-local-variable-values '((eval org-content 4)))
 '(vc-annotate-background "#1f2430")
 '(vc-annotate-color-map
   (list
    (cons 20 "#bae67e")
    (cons 40 "#d1e07e")
    (cons 60 "#e8da7f")
    (cons 80 "#ffd580")
    (cons 100 "#ffc573")
    (cons 120 "#ffb666")
    (cons 140 "#ffa759")
    (cons 160 "#f0af90")
    (cons 180 "#e2b7c7")
    (cons 200 "#d4bfff")
    (cons 220 "#deacd2")
    (cons 240 "#e899a5")
    (cons 260 "#f28779")
    (cons 280 "#bb6c63")
    (cons 300 "#84514e")
    (cons 320 "#4d3639")
    (cons 340 "#1c1e23")
    (cons 360 "#1c1e23")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rustic-compilation-column ((t (:inherit compilation-column-number))))
 '(rustic-compilation-line ((t (:foreground "LimeGreen")))))
