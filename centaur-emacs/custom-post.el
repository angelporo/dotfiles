;;; custom-post.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;; Copyright (C) 2006-2021 Vincent Zhang

;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;
;;; Code:

(defun setupEmacs29BindBuffer ()
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.wxml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  )


(when emacs/>=29p
  (setupEmacs29BindBuffer)
  )
(add-to-list 'default-frame-alist '(undecorated-round . t))


;; (use-package pyim
;;   :ensure t
;;   :demand t
;;   :init
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (pyim-restart-1 t)))
;;   :hook ((js-mode js2-mode css-mode sgml-mode web-mode tsx-ts-mode typescript-ts-mode) . toggle-input-method)
;;   :config
;;   ;; 我使用全拼
;;   (setq pyim-default-scheme 'xiaohe-shuangpin)
;;   (setq default-input-method "pyim")
;;   (use-package pyim-basedict
;;     :ensure t
;;     :config
;;     ;; 加载 basedict 拼音词库。
;;     ;; (pyim-basedict-enable)
;;     )

;;   (add-to-list 'load-path "~/elisp/pyim-tsinghua-dict")
;;   (require 'pyim-tsinghua-dict)
;;   (pyim-tsinghua-dict-enable)


;;   ;; 设置 pyim 探针
;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。这段代码使用
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-program-mode
;;                   pyim-probe-dynamic-english
;;                   pyim-probe-auto-english
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))

;;   ;; ;; 开启代码搜索中文功能（比如拼音，五笔码等）
;;   ;; (pyim-isearch-mode 1)
;;   ;; 设置 pyim 是否使用云拼音
;;   (setq pyim-cloudim 'baidu)
;;   ;; 关闭当前buffer,自动搜索词汇功能
;;   (setq pyim-candidates-search-buffer-p nil)
;;   (setq pyim-dicts
;;         '(
;;           (:name "dict7" :file "~/elisp/scel2pyim/前端工程师必备词库.pyim")
;;           (:name "dict2" :file "~/elisp/scel2pyim/开发大神专用词库【官方推荐】.pyim")
;;           (:name "dict3" :file "~/elisp/scel2pyim/实用IT词汇.pyim")
;;           (:name "dict4" :file "~/elisp/scel2pyim/编程开发.pyim")
;;           (:name "dict5" :file "~/elisp/scel2pyim/前端开发常用词库.pyim")
;;           (:name "dict6" :file "~/elisp/scel2pyim/物流词汇大全【官方推荐】.pyim")
;;           ))


;;   ;; 选词框显示5个候选词
;;   (setq pyim-page-length 9)
;;   ;; 模糊音
;;   (setq pyim-pinyin-fuzzy-alist
;;         '(("z" "zh")
;;           ("c" "ch")
;;           ("s" "sh")
;;           ("en" "eng")
;;           ("in" "ing")))
;;   ;; 让 Emacs 启动时自动加载 pyim 词库
;;   (global-set-key (kbd "M-l") 'pyim-convert-string-at-point)
;;   )


(defun start-centaur-bind-keys ()
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "C-a") 'beginning-of-line)
  (global-set-key (kbd "C-e") 'end-of-line)
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+)
  (global-set-key (kbd "C-i") 'yas-expand)
  )

(start-centaur-bind-keys)

(setq counsel-ag-base-command '(
                                "ag"
                                "--vimgrep" "%s"
                                "--ignore" "*node_modules*"
                                ))
(setenv "LANG" "Chinese")


(use-package lsp-bridge
  :ensure nil
  :load-path "~/elisp/lsp-bridge"
  :hook (prog-mode . lsp-bridge-mode)
  :bind (:map lsp-bridge-mode
         ("C-s-j" . lsp-bridge-jump-to-next-diagnostic) ;显示下一个错误
         ("C-s-k" . lsp-bridge-jump-to-prev-diagnostic) ;显示上一个错误
         ("C-s-n" . lsp-bridge-popup-documentation-scroll-up) ;向下滚动文档
         ("C-s-p" . lsp-bridge-popup-documentation-scroll-down) ;向上滚动文档
         ("C-c e l" . lsp-bridge-list-diagnostics)
         ("C-s-u" . lsp-bridge-ignore-current-diagnostic) ;插入注视忽略当前诊断
         ("M-." . lsp-bridge-find-def)
         ("M-," . lsp-bridge-find-def-return)
         ("M-?" . lsp-bridge-find-references)
         ("C-c RET" . lsp-bridge-popup-documentation)
         ("C-c m" . lsp-bridge-rename)
         ("M-RET" . lsp-bridge-code-action)
         )
  :config
  (setq acm-enable-tabnine nil)
  (setq acm-enable-codeium nil)
  (setq acm-enable-yas nil)
  (setq acm-enable-tempel nil)
  (setq lsp-bridge-auto-format-code-idle -1)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-enable-auto-format-code nil)
  (setq acm-backend-yas-candidates-number 4)
  (setq lsp-bridge-multi-lang-server-extension-list '((("less") . "css_emmet")
                                                      (("vue") . "volar_emmet")
                                                      (("html") . "html_emmet")
                                                      (("tsx") . "tsx_emmet")
                                                      ))

  ;; 这些字符的后面不再弹出补全菜单
  (setq lsp-bridge-completion-hide-characters '("%" ":" ";" "(" ")" "[" "]" "{" "}" "," "=" ">" "\""))
  (global-lsp-bridge-mode)
  )

(use-package ag
  :ensure t
  )


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

(defvar sis-context-hooks
  '(post-command-hook)
  "Hooks trigger the set of input source following context.")

(use-package sis
  :ensure t
  :init
  ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
  ;; (setq sis-respect-go-english-triggers
  ;;       (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入
  ;;       sis-respect-restore-triggers
  ;;       (list 'isearch-exit 'isearch-abort))
                                        ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
  (add-hook 'emacs-startup-hook
            (lambda ()
              (sis-set-english)))
  :config
  (setq sis-english-source "com.apple.keylayout.ABC")
  (setq sis-other-source "com.sogou.inputmethod.sogou.pinyin")
  ;; (sis-ism-lazyman-config
  ;;  "com.apple.keylayout.ABC"
  ;;  ;; "com.apple.inputmethod.SCIM.Shuangpin" ;; 苹果自带双拼输入法这里
  ;;  "com.sogou.inputmethod.sogou.pinyin" ;; 搜狗输入法
  ;; (global-set-key (kbd "M-<spc>") 'sis-switch) ; 切换输入法

  (sis-global-cursor-color-mode t)

  ;; enable the /respect/ mode buffer 输入法状态记忆模式
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)  ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文

  ;; 特殊定制
  (setq sis-do-set
        (lambda(source) (start-process "set-input-source" nil "macism" source "40000")))

  (setq sis-prefix-override-keys (list "C-c" "C-x" "C-h" "C-c e"))

  (add-hook 'org-capture-mode-hook #'sis-set-other)
  (setq sis-default-cursor-color "#02C389" ; 英文光标色
        sis-other-cursor-color "#F95B5B" ; 中文光标色
        sis-inline-tighten-head-rule 'zero ; 删除头部空格，默认1，删除一个空格，1/0/'all
        sis-inline-tighten-tail-rule 'zero ; 删除尾部空格，默认1，删除一个空格，1/0/'all
        sis-inline-with-english t ; 默认是t, 中文context下输入<spc>进入内联英文
        sis-inline-with-other t) ; 默认是nil，而且prog-mode不建议开启, 英文context下输入<spc><spc>进行内联中文


  (defvar my-debounce-interval 0.8
    "Debounce interval for cursor movement in seconds.")

  (defvar my-debounce-timer nil
    "Timer used for debouncing cursor movement.")

  (defvar my-last-position nil
    "Last known position of the cursor.")

  (defun my-debounce-cursor-movement ()
    "Debounced version of cursor movement."
    (when (timerp my-debounce-timer)
      (cancel-timer my-debounce-timer))
    (setq my-debounce-timer (run-with-idle-timer my-debounce-interval nil #'my-real-cursor-movement)))

  (defun my-real-cursor-movement ()
    "Actual handling of cursor movement."
    (when (and (bound-and-true-p my-last-position)
               (equal my-last-position (point)))
      (run-hooks 'sis-context-shooks))
    (setq my-last-position (point)))

  (add-hook 'post-command-hook #'my-debounce-cursor-movement)

  (defun my-set-last-position ()
    "Set the last known position of the cursor."
    (setq my-last-position (point)))

  (add-hook 'pre-command-hook #'my-set-last-position)



  )

;; (setq prettier-js-args '(
;;                          ;; 尽可能尾随逗号
;;                          "--trailing-comma" "all"
;;                          ;; { foo: bar }
;;                          "--bracket-spacing" "false"
;;                          ;; 尽可能省略括号。例子：x => x
;;                          "--arrow-parens" "avoid"
;;                          ;; 指定打印机将环绕的行长度。
;;                          "--print-width" "100"
;;                          ;; 指定每个缩进级别的空格数。
;;                          "--tab-width" "2"
;;                          ;; 在每条语句的末尾添加一个分号。
;;                          "--no-semi" "true"
;;                          ;; 在 JSX 中使用单引号代替双引号。
;;                          "--jsx-single-quote" "true"
;;                          ;; 将>多行 JSX 元素的 放在最后一行的末尾，而不是单独放在下一行（不适用于自关闭元素）。
;;                          "--jsx-bracket-same-line" "true"
;;                          ))



;; (with-eval-after-load 'company
;;   (dolist (map (list company-active-map company-search-map))
;;     (define-key map (kbd "C-n") nil)
;;     (define-key map (kbd "C-p") nil)
;;     (define-key company-active-map (kbd "M-q") 'company-other-backend)
;;     (define-key company-active-map (kbd "C-i") 'yas-expand)
;;     (define-key company-active-map (kbd "C-n") 'next-line)
;;     (define-key company-active-map (kbd "C-p") 'previous-line)
;;     (define-key map (kbd "M-n") #'company-select-next)
;;     (define-key map (kbd "M-p") #'company-select-previous))
;;   )


;; (use-package flutter
;;   :after dart-mode
;;   :bind (:map dart-mode-map
;;          ("C-M-x" . #'flutter-run-or-hot-reload))
;;   :custom
;;   (flutter-sdk-path "/Users/angel/.flutter"))


(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (css-mode . emmet-mode)
  (typescript-mode . emmet-mode)
  )
