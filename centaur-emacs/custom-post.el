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


(defun my/toggle-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90)) ;; 分别为 frame 获得焦点和失去焦点的不透明度
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  )

(defun setupEmacs29BindBuffer ()
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.wxml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

  (add-hook 'typescript-ts-mode-hook 'prettier-mode)
  (add-hook 'tsx-ts-mode-hook 'prettier-mode)
  (add-hook 'js2-mode-hook 'prettier-mode)
  (add-hook 'web-mode-hook 'prettier-mode)
  (add-hook 'css-mode-hook 'prettier-mode)

  )

(defun start-centaur-bind-keys ()
  (global-set-key (kbd "C-a") 'beginning-of-line)
  ;; (global-set-key (kbd "C-e") 'end-of-line)
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+)
  ;; (global-set-key (kbd "C-i") 'yas-expand)
  (global-set-key (kbd "C-c i") 'project-find-file)
  (setq consult-preview-key nil)
  (global-set-key (kbd "M-RET") 'eglot-code-actions)
  ;; (global-set-key (kbd "M-s") 'save-buffer)
  )

(when emacs/>=29p
  (setupEmacs29BindBuffer)
  (start-centaur-bind-keys)


  (setq inhibit-compacting-font-caches t)

  (setq-default cursor-type 'box)  ;; 你可以选择光标样式，例如 box、bar、hollow 等

  (set-cursor-color "red")  ;; 设置光标颜色为红色

  ;; 关闭平滑滚动效果
  (ultra-scroll-mode -1)

  (add-to-list 'default-frame-alist '(undecorated . t))
  ;; (my/toggle-transparency)
  )


(use-package emacs
  :init
  ;; minibuffer 不显示光标.
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; M-x 只显示当前 mode 支持的命令。
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; 开启 minibuffer 递归编辑。
  (setq enable-recursive-minibuffers t)
  )

;; 删除文件时, 将文件移动到回收站。
(use-package osx-trash
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq-default delete-by-moving-to-trash t))


;; 关闭日志打印，不卡emacs
(setq-default eglot-events-buffer-size 0)


(use-package lsp-bridge
  :ensure nil
  :load-path "~/elisp/lsp-bridge"
  :hook (prog-mode . lsp-bridge-mode)
  :bind (:map lsp-bridge-mode
         ("C-s-n" . lsp-bridge-popup-documentation-scroll-up) ; 向下滚动文档
         ("C-s-p" . lsp-bridge-popup-documentation-scroll-down) ; 向上滚动文档
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
  (setq acm-enable-yas t)
  (setq acm-enable-copilot t)
  (setq acm-enable-tempel nil)
  (setq lsp-bridge-auto-format-code-idle -1)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-enable-auto-format-code nil)
  (setq acm-backend-yas-candidate-min-length 3)
  (setq acm-backend-yas-candidates-number 4)
  (setq lsp-bridge-multi-lang-server-extension-list '((("less") . "css_emmet")
                                                      (("vue") . "volar_emmet")
                                                      (("html") . "html_emmet")
                                                      ))

  ;;; lsp-bridge
  ;; M-j 被预留给 pyim 使用。
  (define-key acm-mode-map (kbd "M-j") nil)

  ;; 这些字符的后面不再弹出补全菜单
  (setq lsp-bridge-completion-hide-characters '("%" ":" ";" "(" ")" "[" "]" "{" "}" "," "=" ">" "\""))
  (global-lsp-bridge-mode)
  )







;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :ensure t
;;   :config
;;                                         ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;;   (setenv "ANTHROPIC_API_KEY" "sk-...")
;;                                         ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
;;   (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
;;   :custom
;;                                         ; See the Configuration section below
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "sonnet"))


(use-package rime
  :custom
  (rime-user-data-dir "~/Library/Rime/")
  (rime-librime-root "~/.config/emacs/librime/dist")
  (rime-emacs-module-header-root "/usr/local/opt/emacs-plus@30/include")
  :hook
  (emacs-startup . (lambda () (setq default-input-method "rime")))
  :bind
  (
   :map rime-active-mode-map
   ;; 在已经激活 Rime 候选菜单时，强制在中英文之间切换，直到按回车
   ("M-j" . 'rime-inline-ascii)
   :map rime-mode-map
   ;; ;; 强制切换到中文模式
   ("M-j" . 'rime-force-enable)
   ;; 下面这些快捷键需要发送给 rime 来处理, 需要与 default.custom.yaml 文件中的 key_binder/bindings 配置相匹配。

   ;; 中英文切换
   ("C-," . 'rime-send-keybinding)

   ;; 中英文标点切换
   ("C-." . 'rime-send-keybinding)

   ;; 菜单
   ("C-+" . 'rime-send-keybinding)

   ;; 全半角切换
   ;; ("C-," . 'rime-send-keybinding)
   )
  :config
  ;; 在 modline 高亮输入法图标, 可用来快速分辨分中英文输入状态。
  (setq mode-line-mule-info '((:eval (rime-lighter))))

  ;; 将如下快捷键发送给 rime，同时需要在 rime 的 key_binder/bindings 的部分配置才会生效。
  (add-to-list 'rime-translate-keybindings "C-h") ;; 删除拼音字符
  (add-to-list 'rime-translate-keybindings "C-d")
  (add-to-list 'rime-translate-keybindings "C-k")
  (add-to-list 'rime-translate-keybindings "C-a") ;; 跳转到第一个拼音字符
  (add-to-list 'rime-translate-keybindings "C-e") ;; 跳转到最后一个拼音字符
  ;; support shift-l, shift-r, control-l, control-r, 只有当使用系统 RIME 输入法时才有效
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-deactivate-when-exit-minibuffer nil)

  (defun rime-predicate-avy-p ()
    (bound-and-true-p avy-command))


  ;; 临时英文模式, 该列表中任何一个断言返回 t 时自动切换到英文。如何 rime-inline-predicates 不为空，
  ;; 则当其中任意一个断言也返回 t 时才会自动切换到英文（inline 等效于 ascii-mode）。
  ;; 自定义 avy 断言函数.
  (setq rime-disable-predicates
        '(rime-predicate-ace-window-p
          rime-predicate-avy-p
          rime-predicate-hydra-p
          rime-predicate-after-ascii-char-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-punctuation-after-ascii-p
          rime-predicate-auto-english-p
          ))

  (setq rime-show-candidate 'posframe)
  (setq default-input-method "rime")

  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 4))



  ;; 部分 major-mode 关闭 RIME 输入法。
  (defadvice switch-to-buffer (after activate-input-method activate)
    (if (or (string-match "vterm-mode" (symbol-name major-mode))
            (string-match "dired-mode" (symbol-name major-mode))
            (string-match "image-mode" (symbol-name major-mode))
            (string-match "compilation-mode" (symbol-name major-mode))
            (string-match "isearch-mode" (symbol-name major-mode))
            (string-match "minibuffer-mode" (symbol-name major-mode))
            )
        (activate-input-method nil)
      (activate-input-method "rime")))

  (defvar im-cursor-color "Orange"
    "The color for input method.")

  (defvar im-default-cursor-color (frame-parameter nil 'cursor-color)
    "The default cursor color.")

  (defun im--chinese-p ()
    "Check if the current input state is Chinese."
    (if (featurep 'rime)
        (and (rime--should-enable-p)
             (not (rime--should-inline-ascii-p))
             current-input-method)
      current-input-method))

  (defun im-change-cursor-color ()
    "Set cursor color depending on input method."
    (interactive)
    (set-cursor-color (if (im--chinese-p)
                          im-cursor-color
                        im-default-cursor-color)))

  (define-minor-mode cursor-chg-mode
    "Toggle changing cursor color.
With numeric ARG, turn cursor changing on if ARG is positive.
When this mode is on, `im-change-cursor-color' control cursor changing."
    :init-value nil :global t :group 'frames
    (if cursor-chg-mode
        (add-hook 'post-command-hook 'im-change-cursor-color)
      (remove-hook 'post-command-hook 'im-change-cursor-color)))

  (cursor-chg-mode 1)
  )

(setq counsel-ag-base-command '(
                                "ag"
                                "--vimgrep" "%s"
                                "--ignore" "*node_modules*"
                                ))

;; (use-package emmet-mode
;;   :ensure t
;;   :hook ((typescript-ts-mode  tsx-ts-mode web-mode) . emmet-mode)
;;   )


(use-package ag
  :ensure t
  )
