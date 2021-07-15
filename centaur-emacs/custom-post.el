;;; 开启 dired 修改文件权限功能
(setq wdired-allow-to-change-permissions t
      lsp-ui-doc-position 'top
      )

(add-to-list 'lsp-language-id-configuration '(".*\\.less" . "css"))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; (use-package company-tabnine
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends #'company-tabnine)
;;   :config
;;   (setq company-idle-delay 0.0
;;         company-tooltip-idle-delay 0.0
;;         company-show-numbers t
;;         company-minimum-prefix-length 1
;;         lsp-headerline-breadcrumb-mode t)
;;   )

(setq prettier-js-args '(
                         "--trailing-comma" "all"
                         "--bracket-spacing" "false"
                         ))

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

(setq centaur-completion-style 'childframe)


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
