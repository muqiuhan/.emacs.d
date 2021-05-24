;; 启动设置
(defconst *start-server* nil) ;; 启动打开server
(defconst *start-package-init* t) ;; 启动初始化包
(defconst *frame-start* 2) ;; 窗体启动时 (:max 1 :full 2 :nil 默认)

;; 窗体设置
(defconst *frame-font* "Sarasa Mono Slab K 17") ;; 字体设置
(defconst *frame-menu-bar* 0) ;; 是否显示菜单栏
(defconst *frame-tool-bar* 0) ;; 是否显示工具栏
(defconst *frame-tab-bar* 0) ;; 是否显示标签栏
(defconst *frame-scroll-bar* 0) ;; 是否显示滚动条
(defconst *frame-battery-mode* 1) ;; 是否显示电池信息
(defconst *frame-time-mode* 1) ;; 是否显示当前日期信息
(defconst *frame-size-indication-mode* 1) ;; 大小显示
(defconst *frame-theme* 'doom-horizon) ;; 主题设置

;; 编辑器设置
(defconst *editor-evil* nil) ;; 是否使用vim键位
(defconst *editor-high-line* t) ;; 是否高亮当前行
(defconst *editor-line-number* t) ;; 是否显示行号
(defconst *editor-cursor-type* 'block) ;; 鼠标样式
(defconst *editor-modeline-style* 1) ;; 使用的modeline (:nil default :1 doom-modeline :2 powerline)
(defconst *editor-smooth-scrolling* t) ;; 是否启用平滑滚动 (摁住shift后加快滚动)
(defconst *editor-smooth-scrolling-size* 2) ;; 平滑滚动幅度
(defconst *editor-smooth-scrolling-shift-size* 4) ;; 加速滚动幅度
(defconst *editor-which-function-mode* t) ;; 是否在状态栏显示当前所在函数

;; 全局快捷键设置
(defconst *treemacs* "C-x t t") ;; treemacs快捷键
(defconst *vterm* "C-c t") ;; vterm快捷键

;; 有道翻译的显示模式
;; youdao-dictionary-search-at-point+ 使用默认的弹窗显示
;; youdao-dictionary-search-at-point-tooltip 使用tool-tip显示
;; youdao-dictionary-search-at-point-posframe 使用posframe显示
;; youdao-dictionary-search-at-point 使用eldoc显示
(defconst *youdao-dictionary-mode* 'youdao-dictionary-search-at-point)
(defconst *youdao-dictionary* "C-x y") ;; 翻译快捷键
(defconst *youdao-dictionary-play* "C-x t y") ;; 读出句子快捷键

;; org-mode配置
(defconst *org-mode-bullets* t) ;; 是否打开org-mode美化

;; 语言配置
;; C/C++
(defconst *lang-enable-c/c++* t) ;; 是否打开C/C++配置
;; C/C++的补全方案
;; nil -> 使用默认的补全
;; 1 -> 使用nox补全（轻量级的LSP）
;; 2 -> 使用irony补全
;; 3 -> 使用Lsp补全
(defconst *lang-c/c++-completion* 2)
(defconst *lang-c/c++-flycheck* t) ;; 是否打开语法检查

;; Racket
(defconst *lang-enable-racket* t)
