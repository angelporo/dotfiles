;;; 开启 dired 修改文件权限功能
(setq wdired-allow-to-change-permissions t)

(add-hook 'prog-mode-hook
          (lambda()
            (display-line-numbers-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(flyspell-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(global-diff-hl-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(diff-hl-flydiff-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(highlight-indent-guides-mode -1)) )
(add-hook 'typescript-mode-hook (lambda ()  ( prettier-js-mode t)))
(add-hook 'js-mode-hook (lambda ()  ( prettier-js-mode t)))



(use-package sis
  :ensure t
  :init
  ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
  (setq sis-respect-go-english-triggers
        (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入
        sis-respect-restore-triggers
        (list 'isearch-exit 'isearch-abort)) ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
  :config
  (sis-ism-lazyman-config

   ;; English input source may be: "ABC", "US" or another one.
   ;; "com.apple.keylayout.ABC"
   "com.apple.keylayout.US"

   ;; Other language input source: "rime", "sogou" or another one.
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "com.sogou.inputmethod.sogou.pinyin" ;;搜狗输入法
   ;; "com.apple.inputmethod.SCIM.Shuangpin" ;; 苹果自带双拼输入法
   )
  ;; enable the /cursor color/ mode 中英文光标颜色模式
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode buffer 输入法状态记忆模式
  ;; (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-context-mode )
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t) ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文
  ;; (global-set-key (kbd "C-M-<spc>") 'sis-switch) ; 切换输入法
  ;; 特殊定制
  (setq sis-do-set
        (lambda(source) (start-process "set-input-source" nil "macism" source "50000")))
  (setq sis-default-cursor-color "#E55D9C" ; 英文光标色
        sis-other-cursor-color "#FF2121" ; 中文光标色
        ;; sis-inline-tighten-head-rule 'all ; 删除头部空格，默认1，删除一个空格，1/0/'all
        sis-inline-tighten-tail-rule 'all ; 删除尾部空格，默认1，删除一个空格，1/0/'all
        sis-inline-with-english t ; 默认是t, 中文context下输入<spc>进入内联英文
        sis-inline-with-other t) ; 默认是nil，而且prog-mode不建议开启, 英文context下输入<spc><spc>进行内联中文
  ;; 特殊 buffer 禁用 sis 前缀,使用 Emacs 原生快捷键  setqsis-prefix-override-buffer-disable-predicates
  (setq sis-prefix-override-buffer-disable-predicates
        (list 'minibufferp
              (lambda (buffer) ; magit revision magit的keymap是基于text property的，优先级比sis更高。进入 magit 后，disable sis 的映射
                (sis--string-match-p "^magit-revision:" (buffer-name buffer)))
              (lambda (buffer) ; special buffer，所有*打头的buffer，但是不包括*Scratch* *New, *About GNU等buffer
                (and (sis--string-match-p "^\*" (buffer-name buffer))
                     (not (sis--string-match-p "^\*About GNU Emacs" (buffer-name buffer))) ; *About GNU Emacs" 仍可使用 C-h/C-x/C-c 前缀
                     (not (sis--string-match-p "^\*New" (buffer-name buffer)))
                     (not (sis--string-match-p "^\*Scratch" (buffer-name buffer))))))) ; *Scratch*  仍可使用 C-h/C-x/C-c 前缀
  )
