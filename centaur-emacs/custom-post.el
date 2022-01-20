;;; custom.post.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
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


;;; 开启 dired 修改文件权限功能
(setq dired-allow-to-change-permissions t
      )

(add-to-list 'lsp-language-id-configuration '(".*\\.less" . "css"))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))

(setq scroll-step 0
      scroll-conservatively 0
      lsp-ui-doc-position 'top
      ns-alternate-modifier 'super
      ns-command-modifier 'meta
      centaur-icon t ; Display icons or not: t or nil
      )


(use-package company-tabnine
  :ensure t
  :init
  (add-to-list 'company-backends #'company-tabnine)
  :config
  (setq company-idle-delay 0.0
        company-tooltip-idle-delay 0.0
        company-show-numbers t
        company-minimum-prefix-length 1
        company-tabnine-max-restart-count 40
        company-tabnine-max-num-results 30
        lsp-headerline-breadcrumb-mode t
        )
  )

;; workaround for company-transformers
(setq company-tabnine--disable-next-transform nil)
(defun my-company--transform-candidates (func &rest args)
  (if (not company-tabnine--disable-next-transform)
      (apply func args)
    (setq company-tabnine--disable-next-transform nil)
    (car args)))

(defun my-company-tabnine (func &rest args)
  (when (eq (car args) 'candidates)
    (setq company-tabnine--disable-next-transform t))
  (apply func args))

(advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
(advice-add #'company-tabnine :around #'my-company-tabnine)



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

;; (defun sanityinc/disable-features-during-macro-call (orig &rest args)
;;   "When running a macro, disable features that might be expensive.
;; ORIG is the advised function, which is called with its ARGS."
;;   (let (post-command-hook
;;         font-lock-mode
;;         (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
;;     (apply orig args)))

;; (advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)


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


(use-package add-node-modules-path
  :ensure t
  :config
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path))
  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))
  )


(use-package company
  :config
  (define-key company-active-map (kbd "M-q") 'company-other-backend)
  (define-key company-active-map (kbd "C-i") 'yas-expand)
  (define-key company-active-map (kbd "C-n") 'next-line)
  (define-key company-active-map (kbd "C-p") 'previous-line)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        去除web-mode中自动缩进      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook web-mode-hook (lambda ()
                          (electric-indent-local-mode -1)
                          ))



(use-package sis
  :init
  ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
  (setq sis-respect-go-english-triggers
        (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入
        sis-respect-restore-triggers
        (list 'isearch-exit 'isearch-abort))   ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
  :config
  (sis-ism-lazyman-config

   ;; English input source may be: "ABC", "US" or another one.
   "com.apple.keylayout.ABC"
   ;; "com.apple.keylayout.US"

   ;; Other language input source: "rime", "sogou" or another one.
   ;; "im.rime.inputmethod.Squirrel.Rime"
   ;; "com.sogou.inputmethod.sogou.pinyin" ;;搜狗输入法
   "com.apple.inputmethod.SCIM.Shuangpin" ;; 苹果自带双拼输入法
   )
  ;; enable the /cursor color/ mode 中英文光标颜色模式
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode buffer 输入法状态记忆模式
  ;; (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-context-mode nil)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode nil) ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文
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
