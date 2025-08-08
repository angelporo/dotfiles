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

;; 解决GUI Emacs与终端环境变量不一致的问题
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil) ; Faster initialization
    (setq exec-path-from-shell-variables
          '("PATH" "MANPATH" "LANG" "LC_ALL" "GOPATH" "GOROOT" "NVM_DIR" "OPENROUTER_API_KEY"
            "JAVA_HOME" "ANDROID_HOME" "DEEPSEEK_API_KEY" "PYTHONPATH"))
    :config
    (exec-path-from-shell-initialize)


    ;; Cache the values to avoid future shell calls
    (dolist (var exec-path-from-shell-variables)
      (setenv var (getenv var)))))

(defun my/setup-emacs29-bindings ()
  "Setup file associations and hooks for Emacs 29+."
  ;; (dolist (mode '(("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)
  ;;                 ("\\.rs\\'" . rust-ts-mode)
  ;;                 ("\\.wxml\\'" . web-mode)
  ;;                 ("\\.ts\\'" . tsx-ts-mode)
  ;;                 ("\\.js\\'" . tsx-ts-mode)
  ;;                 ("\\.tsx\\'" . tsx-ts-mode)
  ;;                 ("\\.less\\'" . less-css-mode)
  ;;                 ("\\.ya?ml\\'" . yaml-ts-mode)))
  ;;   (add-to-list 'auto-mode-alist mode))

  (dolist (hook '(typescript-ts-mode-hook
                  tsx-ts-mode-hook
                  js2-mode-hook
                  web-mode-hook
                  css-mode-hook))
    (add-hook hook 'prettier-mode)))

(defun my/setup-keybindings ()
  "Setup global keybindings."
  (let ((map global-map))
    (define-key map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
    (define-key map (kbd "C-c i") 'project-find-file)
    (define-key map (kbd "M-RET") 'eglot-code-actions))

  ;; Disable consult preview
  (setq consult-preview-key nil))

(when emacs/>=29p
  (my/setup-emacs29-bindings)
  (my/setup-keybindings)

  ;; UI enhancements
  (setq inhibit-compacting-font-caches t) ; Better font rendering
  (setq-default cursor-type 'box)         ; Box cursor
  (set-cursor-color "red")                ; Red cursor color

  ;; Disable smooth scrolling for better performance
  (ultra-scroll-mode -1))


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


(use-package emigo
  :init
  (unless (package-installed-p 'emigo)
    (package-vc-install "https://github.com/MatthewZMD/emigo.git"))

  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  ;; Encourage using OpenRouter with Deepseek
  (emigo-model "deepseek-reasoner")
  (emigo-base-url "https://api.deepseek.com/v1")
  (emigo-api-key (getenv "DEEPSEEK_API_KEY")))


(use-package aidermacs
  :defer t  ; Defer loading until first use
  :commands (aidermacs-transient-menu)
  :bind (("C-c a" . aidermacs-transient-menu))
  :init
  ;; Cache API key at startup
  (setq aider-api-key (getenv "DEEPSEEK_API_KEY"))
  (setenv "AIDER_CHAT_LANGUAGE" "Chinese")
  :custom
  ;; (aidermacs-default-model "deepseek/deepseek-reasoner")
  (aidermacs-default-model "deepseek/deepseek-chat")
  )


(use-package lsp-bridge
  :ensure nil
  :init
  (unless (package-installed-p 'lsp-bridge)
    (package-vc-install "https://github.com/manateelazycat/lsp-bridge.git"))
  :hook (prog-mode . (lambda ()
                       (when (derived-mode-p 'prog-mode)
                         (lsp-bridge-mode))))
  :bind (:map lsp-bridge-mode
         ("C-s-n" . lsp-bridge-popup-documentation-scroll-up) ; 向下滚动文档
         ("C-s-p" . lsp-bridge-popup-documentation-scroll-down) ; 向上滚动文档
         ("M-." . lsp-bridge-find-def)
         ("M-," . lsp-bridge-find-def-return)
         ("M-?" . lsp-bridge-find-references)
         ("C-c RET" . lsp-bridge-popup-documentation)
         ;; ("C-c m" . lsp-bridge-rename)
         ("M-RET" . lsp-bridge-code-action)
         )
  :config
  ;; (setq lsp-bridge-python-command "~/.pyenv/versions/3.8.18/bin/python3")
  (setq acm-enable-tabnine t)
  (setq acm-enable-codeium nil)
  (setq acm-enable-yas nil)
  (setq acm-enable-copilot t)
  (setq acm-enable-tempel nil)
  (setq lsp-bridge-auto-format-code-idle nil)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-enable-auto-format-code nil)
  (setq acm-backend-yas-candidate-min-length 3)
  (setq acm-backend-yas-candidates-number 4)
  (setq acm-backend-lsp-candidate-min-length 2)
  (setq acm-backend-search-file-words-max-number 7)
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


(use-package rime
  :custom
  (rime-user-data-dir "~/Library/Rime/")
  (rime-librime-root "~/.config/emacs/librime/dist")
  (rime-emacs-module-header-root "/usr/local/opt/emacs-plus@31/incl/include")
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
          rime-predicate-hydra-p
          ;; rime-predicate-after-ascii-char-p
          ;; rime-predicate-after-alphabet-char-p
          ;; rime-predicate-prog-in-code-p
          ;; rime-predicate-punctuation-after-space-cc-p
          ;; rime-predicate-punctuation-after-ascii-p
          ;; rime-predicate-auto-english-p
          ;; rime-predicate-avy-p
          ))

  (setq rime-show-candidate 'posframe)
  (setq default-input-method "rime")

  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 4))



  ;; 部分 major-mode 关闭 RIME 输入法。
  ;; 定义建议函数
  (defvar my-disable-input-method-modes
    '(vterm-mode dired-mode image-mode compilation-mode
                 isearch-mode minibuffer-inactive-mode)
    "Major modes where input method should be disabled.")

  (defun my-activate-input-method-after-switch (&rest _args)
    "Activate input method based on current mode."
    (let ((disable (cl-some #'derived-mode-p my-disable-input-method-modes)))
      (activate-input-method (unless disable "rime"))))

  ;; 添加建议到 switch-to-buffer 的 :after 位置
  (advice-add 'switch-to-buffer :after #'my-activate-input-method-after-switch)


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
