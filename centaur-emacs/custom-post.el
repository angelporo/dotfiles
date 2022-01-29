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


(add-to-list 'lsp-language-id-configuration '(".*\\.less" . "css"))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))


(use-package company-tabnine
  :ensure t
  :init
  (add-to-list 'company-backends #'company-tabnine)
  (defun company//sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  ;; (defun lsp-after-open-tabnine ()
  ;;   "Hook to attach to `lsp-after-open'."
  ;;   (setq-local company-tabnine-max-num-results 5)
  ;;   ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  ;;   ;; (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
  ;;   )
  (defun company-tabnine-toggle (&optional enable)
    "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
    (interactive)
    (if (or enable (not (memq 'company-tabnine company-backends)))
        (progn
          ;; (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
          (add-to-list 'company-backends #'company-tabnine)
          ;; (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
          (message "TabNine enabled."))
      (setq company-backends (delete 'company-tabnine company-backends))
      (setq company-backends (delete '(company-capf :with company-tabnine :separate) company-backends))
      ;; (remove-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
      (company-tabnine-kill-process)
      (message "TabNine disabled.")))
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :config
  (setq company-idle-delay 0.0
        company-tooltip-idle-delay 0.0
        company-show-numbers t
        company-minimum-prefix-length 1
        company-tabnine-max-restart-count 40
        company-tabnine-max-num-results 30
        lsp-headerline-breadcrumb-mode t
        )
  (company-tabnine-toggle t)
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

(use-package lsp-mode
  :ensure t
  :config
  (global-set-key (kbd "C-M-m") 'lsp-execute-code-action)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        去除web-mode中自动缩进      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook web-mode-hook (lambda ()
                          (electric-indent-local-mode -1)
                          ))
