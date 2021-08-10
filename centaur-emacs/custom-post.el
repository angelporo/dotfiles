;;; 开启 dired 修改文件权限功能
(setq wdired-allow-to-change-permissions t
      lsp-ui-doc-position 'top
      )

(add-to-list 'lsp-language-id-configuration '(".*\\.less" . "css"))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

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
        lsp-headerline-breadcrumb-mode t)
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

(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)


(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
         ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Users/angel/.flutter"))


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
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
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
