;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:
(add-hook 'prog-mode-hook (lambda()
                            (setq scroll-step 0)
                            (setq scroll-conservatively 0)
                            (indent-bars-mode -1)
                            (global-hl-line-mode  -1)
                            (global-subword-mode -1)
                            (global-hungry-delete-mode -1)
                            (flyspell-mode -1)
                            (desktop-save-mode -1)
                            (tabspaces-mode -1)
                            (display-line-numbers-mode -1)
                            (diff-hl-flydiff-mode -1)
                            (global-diff-hl-mode -1)
                            (persistent-scratch-mode -1)
                            (persistent-scratch-autosave-mode -1)
                            (rainbow-delimiters-mode -1)
                            (symbol-overlay-mode -1)
                            (corfu-mode -1)
                            ))

(setq ns-alternate-modifier 'super)
(setq ns-command-modifier 'meta)
(setq centaur-icon t)
(setq centaur-full-name "angelporo")              ; User full name
(setq centaur-mail-address "940079461@qq.com")    ; Email address
(setq centaur-proxy "127.0.0.1:1087")             ; Network proxy
(setq centaur-socks-proxy "127.0.0.1:7897")

(setq centaur-server t)                      ; Enable `server-mode' or not: t or nil

(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'light)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
(setq centaur-completion-style 'childframe)    ; Completion display style: minibuffer or childframe
(setq centaur-dashboard t)                   ; Use dashboard at startup or not: t or nil
(setq centaur-lsp nil)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode web-mode tsx-ts-mode typescript-mode typescript-ts-mode css-mode css-ts-mode markdown-mode )) ; Ignore format on save for some languages
(setq centaur-tree-sitter t)                 ; Enable `tree-sitter' or not: t or nil
(setq centaur-chinese-calendar nil)              ; Use Chinese calendar or not: t or nil
(setq centaur-player nil)                        ; Enable players or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
(setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("SF Mono" "Cascadia Mono" "Cascadia Code" "Fira Code" "Source Code Pro" "Jetbrains Mono"
                           "Hack" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :weight 'heavy'
                                        :height (cond (sys/macp 132)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
             when (font-installed-p font)
             return (progn
                      (set-face-attribute 'mode-line nil :family font :height 120)
                      (when (facep 'mode-line-active)
                        (set-face-attribute 'mode-line-active nil :family font :height 120 ))
                      (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t
                                      (if (< emacs-major-version 28)'symbol 'emoji)
                                      (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.1)))
                      (set-fontset-font t 'han (font-spec :family font))))))

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
(setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
(enable-http-proxy)
;; (enable-socks-proxy)

;; Display on the specified monitor
(when (and (> (length (display-monitor-attributes-list)) 1)
           (> (display-pixel-width) 1920))
  (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(acm-backend-codeium-accept t)
 '(acm-enable-codeium t)
 '(custom-safe-themes
   '("5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" default))
 '(lsp-bridge-diagnostic-max-number 20)
 '(lsp-bridge-python-command "python3")
 '(lsp-bridge-remote-python-command "python3")
 '(lsp-idle-delay 0.2)
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll")
     (eglot-booster :vc-backend Git :url
       "https://github.com/jdtsmith/eglot-booster")))
 '(rime-deactivate-when-exit-minibuffer nil)
 '(safe-local-variable-directories
   '("/Volumes/D/shide/zy/sd-operating-frontEnd/"
     "/Volumes/D/shide/lg/lg-dashboard-web/"))
 '(safe-local-variable-values
   '((web-mode-indent-style . 2) (web-mode-block-padding . 2)
     (web-mode-script-padding . 2) (web-mode-style-padding . 2)))
 '(web-mode-enable-auto-indentation nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
