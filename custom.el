;; 启动设置
(defconst *start-server* t) ;; 启动打开server
(defconst *start-package-init* t) ;; 启动初始化包

;; 窗体设置
(defconst *frame-font* "Sarasa Mono Slab K 16") ;; 字体设置
(defconst *frame-menu-bar* 0) ;; 是否显示菜单栏
(defconst *frame-tool-bar* 0) ;; 是否显示工具栏
(defconst *frame-tab-bar* 0) ;; 是否显示标签栏
(defconst *frame-scroll-bar* 0) ;; 是否显示滚动条
(defconst *frame-battery-mode* 1) ;; 是否显示电池信息
(defconst *frame-time-mode* 1) ;; 是否显示当前日期信息
(defconst *frame-size-indication-mode* 1) ;; 大小显示
(defconst *frame-start* 1) ;; 窗体启动时 (:max 1 :full 2 :nil 默认)
(defconst *frame-theme* 'doom-gruvbox) ;; 主题设置

;; 编辑器设置
(defconst *editor-high-line* t) ;; 是否高亮当前行
(defconst *editor-line-number* t) ;; 是否显示行号
(defconst *editor-cursor-type* 'block) ;; 鼠标样式
(defconst *editor-modeline-style* 1) ;; 使用的modeline (:nil default :1 doom-modeline :2 powerline)
(defconst *editor-smooth-scrolling* t) ;; 是否启用平滑滚动 (摁住shift后加快滚动)
(defconst *editor-smooth-scrolling-size* 2) ;; 平滑滚动幅度
(defconst *editor-smooth-scrolling-shift-size* 4) ;; 加速滚动幅度

;; 全局变量设置
(defconst *treemacs* "C-x t t") ;; treemacs快捷键
(defconst *vterm* "C-c t") ;; vterm快捷键

;; org-mode配置
(defconst *org-mode-bullets* t) ;; 是否打开org-mode美化
