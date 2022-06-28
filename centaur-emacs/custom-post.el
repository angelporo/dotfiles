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
(add-to-list 'load-path (expand-file-name "~/elisp"))

(require 'company-english-helper)

(add-to-list 'load-path (expand-file-name "~/elisp/lsp-bridge"))

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq frame-resize-pixelwise t)
;; (add-to-list 'lsp-language-id-configuration '(".*\\.less" . "css"))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(setq counsel-ag-base-command '(
                                "ag"
                                "--vimgrep" "%s"
                                "--ignore" "*node_modules*"
                                ))

;; (use-package company-tabnine
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends #'company-tabnine)
;;   (defun company//sort-by-tabnine (candidates)
;;     "Integrate company-tabnine with lsp-mode"
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-lsp
;;             candidates-tabnine)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-tabnine))
;;             (push candidate candidates-lsp)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-lsp (nreverse candidates-lsp))
;;         (setq candidates-tabnine (nreverse candidates-tabnine))
;;         (nconc (seq-take candidates-tabnine 3)
;;                (seq-take candidates-lsp 6)))))
;;   (defun lsp-after-open-tabnine ()
;;     "Hook to attach to `lsp-after-open'."
;;     (setq-local company-tabnine-max-num-results 9)
;;     ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
;;     ;; (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
;;     )
;;   (defun company-tabnine-toggle (&optional enable)
;;     "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
;;     (interactive)
;;     (if (or enable (not (memq 'company-tabnine company-backends)))
;;         (progn
;;           ;; (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
;;           (add-to-list 'company-backends #'company-tabnine)
;;           ;; (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
;;           (message "TabNine enabled."))
;;       (setq company-backends (delete 'company-tabnine company-backends))
;;       (setq company-backends (delete '(company-capf :with company-tabnine :separate) company-backends))
;;       ;; (remove-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
;;       (company-tabnine-kill-process)
;;       (message "TabNine disabled.")))
;;   :hook
;;   (kill-emacs . company-tabnine-kill-process)
;;   :config
;;   (setq company-idle-delay 0.0
;;         company-tooltip-idle-delay 0.0
;;         company-show-numbers t
;;         company-minimum-prefix-length 1
;;         company-tabnine-max-restart-count 40
;;         company-tabnine-max-num-results 30
;;         lsp-headerline-breadcrumb-mode t
;;         )
;;   (company-tabnine-toggle t)
;;   )



(use-package sis
  :ensure t
  :init
  ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
  (setq sis-respect-go-english-triggers
        (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入
        sis-respect-restore-triggers
        (list 'isearch-exit 'isearch-abort))   ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
  :config
  (add-hook 'text-mode-hook #'sis-set-other)
  (add-hook 'typescript-mode-hook #'sis-set-english)
  (add-hook 'dashboard-mode-hook #'sis-set-english)
  (sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   ;; "com.apple.inputmethod.SCIM.Shuangpin" ;; 苹果自带双拼输入法
   "com.sogou.inputmethod.sogou.pinyin" ;; 搜狗输入法
   )
  ;; enable the /cursor color/ mode 中英文光标颜色模式
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode buffer 输入法状态记忆模式
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)  ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文
  ;; (global-set-key (kbd "C-M-<spc>") 'sis-switch) ; 切换输入法
  ;; 特殊定制
  (setq sis-do-set
        (lambda(source) (start-process "set-input-source" nil "macism" source "40000")))
  (setq sis-default-cursor-color "#02C389" ; 英文光标色
        sis-other-cursor-color "#F95B5B" ; 中文光标色
        sis-inline-tighten-head-rule 'all ; 删除头部空格，默认1，删除一个空格，1/0/'all
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


(with-eval-after-load 'company
  (dolist (map (list company-active-map company-search-map))
    (define-key map (kbd "C-n") nil)
    (define-key map (kbd "C-p") nil)
    (define-key company-active-map (kbd "M-q") 'company-other-backend)
    (define-key company-active-map (kbd "C-i") 'yas-expand)
    (define-key company-active-map (kbd "C-n") 'next-line)
    (define-key company-active-map (kbd "C-p") 'previous-line)
    (define-key map (kbd "M-n") #'company-select-next)
    (define-key map (kbd "M-p") #'company-select-previous))
  )


(global-set-key (kbd "M-s") 'save-buffer)


;; 尝试 github copilot
;; (load-file "~/elisp/copilot.el/copilot.el")

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el"
;;              :files ("dist" "copilot.el"))
;;   :ensure t
;;   :config
;;   ;; provide completion when typing
;;   (add-hook 'post-command-hook (lambda ()
;;                                  (copilot-clear-overlay)
;;                                  (when (evil-insert-state-p)
;;                                    (copilot-complete)))))
