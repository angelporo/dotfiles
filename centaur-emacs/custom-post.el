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

(add-to-list 'default-frame-alist '(undecorated . t))

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



(use-package rime
  :ensure-system-package
  ("/Applications/SwitchKey.app" . "brew install --cask switchkey")
  :custom
  (rime-user-data-dir "~/Library/Rime/")
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "/usr/local/opt/emacs-plus@29/include")
  :hook
  (emacs-startup . (lambda () (setq default-input-method "rime")))
  :bind
  ( :map rime-active-mode-map
    ;; 在已经激活 Rime 候选菜单时，强制在中英文之间切换，直到按回车。
    ("M-j" . 'rime-inline-ascii)
    :map rime-mode-map
    ;; 强制切换到中文模式
    ("M-j" . 'rime-force-enable)
    ;; 下面这些快捷键需要发送给 rime 来处理, 需要与 default.custom.yaml 文件中的 key_binder/bindings 配置相匹配。
    ;; 中英文切换
    ("C-." . 'rime-send-keybinding)
    ;; 输入法菜单
    ("C-+" . 'rime-send-keybinding)
    ;; 中英文标点切换
    ("C-," . 'rime-send-keybinding)
    ;; 全半角切换
    ;; ("C-," . 'rime-send-keybinding)
    )
  :config
  ;; 在 modline 高亮输入法图标, 可用来快速分辨分中英文输入状态。
  ;; (setq mode-line-mule-info '((:eval (rime-lighter))))
  ;; 将如下快捷键发送给 rime，同时需要在 rime 的 key_binder/bindings 的部分配置才会生效。
  (add-to-list 'rime-translate-keybindings "C-h") ;; 删除拼音字符
  (add-to-list 'rime-translate-keybindings "C-d")
  (add-to-list 'rime-translate-keybindings "C-k")
  (add-to-list 'rime-translate-keybindings "C-a") ;; 跳转到第一个拼音字符
  (add-to-list 'rime-translate-keybindings "C-e") ;; 跳转到最后一个拼音字符
  ;; support shift-l, shift-r, control-l, control-r, 只有当使用系统 RIME 输入法时才有效。
  (setq rime-inline-ascii-trigger 'shift-l)
  ;; 临时英文模式。
  (setq rime-disable-predicates
        '(rime-predicate-auto-english-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-ace-window-p
          rime-predicate-current-uppercase-letter-p
          rime-predicate-punctuation-after-ascii-p
          rime-predicate-prog-in-code-p
          ))
  (setq rime-show-candidate 'posframe)
  ;; (setq default-input-method "rime")

  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 2))

  ;; 部分 major-mode 关闭 RIME 输入法。
  (defadvice switch-to-buffer (after activate-input-method activate)
    (if (or (string-match "vterm-mode" (symbol-name major-mode))
            (string-match "dired-mode" (symbol-name major-mode))
            (string-match "image-mode" (symbol-name major-mode))
            (string-match "minibuffer-mode" (symbol-name major-mode)))
        (activate-input-method nil)
      (activate-input-method "rime")))


  (defvar input-method-cursor-color "Orange"
    "Default cursor color if using an input method.")

  (defvar default-cursor-color (frame-parameter nil 'cursor-color)
    "Default text cursor color.")

  (defun change-cursor-color-on-input-method ()
    "Set cursor color depending on whether an input method is used or not."
    (interactive)
    (set-cursor-color (if (and (rime--should-enable-p)
                               (not (rime--should-inline-ascii-p))
                               current-input-method)
                          input-method-cursor-color
                        default-cursor-color)))

  (add-hook 'post-command-hook 'change-cursor-color-on-input-method)
  )




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
  ;;; lsp-bridge
  ;; M-j 被预留给 pyim 使用。
  (define-key acm-mode-map (kbd "M-j") nil)
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




(setq prettier-js-args '(
                         ;; 尽可能尾随逗号
                         "--trailing-comma" "all"
                         ;; { foo: bar }
                         "--bracket-spacing" "false"
                         ;; 尽可能省略括号。例子：x => x
                         "--arrow-parens" "avoid"
                         ;; 指定打印机将环绕的行长度。
                         "--print-width" "100"
                         ;; 指定每个缩进级别的空格数。
                         "--tab-width" "2"
                         ;; 在每条语句的末尾添加一个分号。
                         "--no-semi" "true"
                         ;; 在 JSX 中使用单引号代替双引号。
                         "--jsx-single-quote" "true"
                         ;; 将>多行 JSX 元素的 放在最后一行的末尾，而不是单独放在下一行（不适用于自关闭元素）。
                         "--jsx-bracket-same-line" "true"
                         ))

(use-package add-node-modules-path
  :ensure t
  :config
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path))
  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)
       ))
  )


;; (use-package flutter
;;   :after dart-mode
;;   :bind (:map dart-mode-map
;;          ("C-M-x" . #'flutter-run-or-hot-reload))
;;   :custom
;;   (flutter-sdk-path "/Users/angel/.flutter"))
