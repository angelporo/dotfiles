;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "angelporo")              ; User full name
(setq centaur-mail-address "940079461@qq.com")    ; Email address
(setq centaur-proxy "127.0.0.1:1087")             ; Network proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
(setq centaur-icon nil)                        ; Display icons or not: t or nil


(add-hook 'prog-mode-hook
          (lambda()
            (display-line-numbers-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(flyspell-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(global-diff-hl-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(diff-hl-flydiff-mode -1)) )
(add-hook 'prog-mode-hook (lambda()(highlight-indent-guides-mode -1)) )
(add-hook 'typescript-mode-hook (lambda ()  ( prettier-js-mode t)))
(add-hook 'js-mode-hook (lambda ()  ( prettier-js-mode t)))


(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'pro)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
;; (setq centaur-completion-style 'childframe)    ; Completion display style: minibuffer or childframe
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
(setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode typescript-mode js-mode css-mode web-mode)) ; Ignore format on save for some languages
(setq centaur-chinese-calendar t)              ; Use Chinese calendar or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 130)
                                                    (sys/win32p 110)
                                                    (t 100))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

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
(proxy-http-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#3e999f" "#c9b4cf" "#8abeb7" "#4d4d4c"])
 '(centaur-package-archives 'melpa)
 '(centaur-theme 'day)
 '(company-tooltip-idle-delay 0)
 '(custom-safe-themes
   '("7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" default))
 '(doom-modeline-mode t)
 '(fci-rule-color "#a3a1a1")
 '(jdee-db-active-breakpoint-face-colors (cons "#f2f2f2" "#4271ae"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f2f2f2" "#718c00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f2f2f2" "#a5a4a5"))
 '(lsp-ui-doc-delay 0.13)
 '(lsp-ui-doc-position 'top)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(objed-cursor-color "#c82829")
 '(pdf-view-midnight-colors (cons "#4d4d4c" "#ffffff"))
 '(rustic-ansi-faces
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#3e999f" "#c9b4cf" "#8abeb7" "#4d4d4c"])
 '(scroll-conservatively 0)
 '(scroll-step 0)
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (list
    (cons 20 "#718c00")
    (cons 40 "#999a00")
    (cons 60 "#c1a800")
    (cons 80 "#eab700")
    (cons 100 "#eda70a")
    (cons 120 "#f19714")
    (cons 140 "#f5871f")
    (cons 160 "#e69659")
    (cons 180 "#d7a594")
    (cons 200 "#c9b4cf")
    (cons 220 "#c88597")
    (cons 240 "#c85660")
    (cons 260 "#c82829")
    (cons 280 "#bf4748")
    (cons 300 "#b66667")
    (cons 320 "#ad8586")
    (cons 340 "#a3a1a1")
    (cons 360 "#a3a1a1")))
 '(vc-annotate-very-old-color nil)
 '(warning-minimum-level :error))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#51afef" :background nil))))
 '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
 '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff6c6b") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#ECBE7B") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff6c6b"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ECBE7B"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
 '(ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
 '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
 '(ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
 '(ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
 '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
 '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
 '(ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-header-face ((t (:inherit diff-header))))
 '(ztreep-leaf-face ((t (:inherit diff-index))))
 '(ztreep-node-face ((t (:inherit font-lock-variable-name-face)))))

;;; custom.el ends here
