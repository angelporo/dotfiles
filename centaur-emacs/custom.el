;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(add-hook 'prog-mode-hook
          (lambda ()
            (setq scroll-step 0)
            (setq scroll-conservatively 0)
            ;; 使用 fboundp 检查函数是否存在，避免未加载包时出错
            (when (fboundp 'indent-bars-mode) (indent-bars-mode -1))
            (when (fboundp 'global-display-line-numbers-mode) (global-display-line-numbers-mode -1))
            (when (fboundp 'global-hl-line-mode) (global-hl-line-mode -1))
            (when (fboundp 'global-subword-mode) (global-subword-mode -1))
            (when (fboundp 'symbol-overlay-mode) (symbol-overlay-mode -1))
            (when (fboundp 'global-hungry-delete-mode) (global-hungry-delete-mode -1))
            (when (fboundp 'flyspell-mode) (flyspell-mode -1))
            (when (fboundp 'desktop-save-mode) (desktop-save-mode -1))
            (when (fboundp 'tabspaces-mode) (tabspaces-mode -1))
            (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode -1))
            (when (fboundp 'diff-hl-flydiff-mode) (diff-hl-flydiff-mode -1))
            (when (fboundp 'global-diff-hl-mode) (global-diff-hl-mode -1))
            (when (fboundp 'persistent-scratch-mode) (persistent-scratch-mode -1))
            (when (fboundp 'persistent-scratch-autosave-mode) (persistent-scratch-autosave-mode -1))
            (when (fboundp 'rainbow-delimiters-mode) (rainbow-delimiters-mode -1))
            (when (fboundp 'prettify-symbols-mode) (prettify-symbols-mode -1))
            (when (fboundp 'region-occurrences-highlighter-mode) (region-occurrences-highlighter-mode -1))
            (when (fboundp 'drag-stuff-mode) (drag-stuff-mode -1))
            (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
            (when (fboundp 'auto-fill-mode) (auto-fill-mode -1))
            (when (fboundp 'abbrev-mode) (abbrev-mode -1))
            (when (fboundp 'diff-hl-show-hunk-mouse-mode) (diff-hl-show-hunk-mouse-mode -1))
            (when (fboundp 'colorful-mode) (colorful-mode -1))
            (when (fboundp 'page-break-lines-mode) (page-break-lines-mode -1))
            (when (fboundp 'anzu-mode) (anzu-mode -1))
            (when (fboundp 'ace-pinyin-mode) (ace-pinyin-mode -1))))

(setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1087")          ; HTTP/HTTPS proxy
;; (setq centaur-socks-proxy "127.0.0.1:1087")    ; SOCKS proxy
(setq centaur-server t)                      ; Enable `server-mode' or not: t or nil
(setq centaur-icon t)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, bfsu, iscas, netease, sjtu, tencent, tuna or ustc
(setq centaur-theme 'night)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
(setq centaur-completion-style 'childframe)    ; Completion display style: minibuffer or childframe
(setq centaur-frame-maximized-on-startup t)    ; Maximize frame on startup or not: t or nil
;; (setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
;; (setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode )) ; Ignore format on save for some languages
(setq centaur-tree-sitter t)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
;; (setq centaur-chinese-calendar t)              ; Support Chinese calendar or not: t or nil
;; (setq centaur-player t)                        ; Enable players or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
(setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("SF Mono" "Menlo" "Monaco" "Fira Code" "Hack"
                           "Source Code Pro" "FiraCode Nerd Font"
                           "Cascadia Code" "Jetbrains Mono"
                           "Hack" "DejaVu Sans Mono" "Consolas")
             when (font-available-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 158)
                                                      (sys/win32p 130)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("SF Mono" "Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-available-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-available-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    ;; (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
    ;;                        "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
    ;;          when (font-available-p font)
    ;;          return (progn
    ;;                   (setq face-font-rescale-alist `((,font . 1.3)))
    ;;                   (set-fontset-font t 'han (font-spec :family font))))
    ))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)
;; (setq package-check-signature nil)

;; Enable proxy
;; (enable-http-proxy)
;; (enable-socks-proxy)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aidermacs-watch-files t)
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
 '(consult-preview-key '("M-."))
 '(custom-safe-themes
   '("5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" default))
 '(ignored-local-variable-values
   '((web-mode-indent-style . 2) (web-mode-block-padding . 2)
     (web-mode-script-padding . 2) (web-mode-style-padding . 2)))
 '(package-vc-selected-packages
   '((vterm-extra :url "https://github.com/Sbozzolo/vterm-extra"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; 输入法光标颜色变化配置
(when (file-exists-p (expand-file-name "lisp/im-cursor-chg.el" user-emacs-directory))
  (require 'im-cursor-chg)
  (setq im-cursor-color "#0077FF"       ; 设置中文输入时的光标颜色为亮红色
        im-default-cursor-color "#FF0000") ; 设置默认光标颜色为亮蓝色
  (cursor-chg-mode 1)
  (message "Input method cursor color change enabled!"))
;;; custom.el ends here
