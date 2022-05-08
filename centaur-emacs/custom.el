;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "angelporo")              ; User full name
(setq centaur-mail-address "940079461@qq.com")    ; Email address
(setq centaur-proxy "127.0.0.1:1087")             ; Network proxy
;; Enable proxy
(proxy-http-enable)

;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
(setq scroll-step 0
      scroll-conservatively 0
      lsp-ui-doc-position 'top
      ns-alternate-modifier 'super
      ns-command-modifier 'meta
      centaur-icon t ; Display icons or not: t or nil
      )

(add-hook 'prog-mode-hook (lambda()
                            (global-subword-mode -1)
                            (global-hungry-delete-mode -1)
                            (highlight-indent-guides-mode -1)
                            (diff-hl-flydiff-mode -1)
                            (global-diff-hl-mode -1)
                            (flyspell-mode -1)
                            (magit-todos-mode -1)
                            (display-line-numbers-mode -1)
                            (global-set-key (kbd "C-a") 'beginning-of-line)
                            (global-set-key (kbd "C-e") 'end-of-line)
                            ))

(setq frame-resize-pixelwise t)

(add-hook 'typescript-mode-hook (lambda ()  ( prettier-js-mode t)))
(add-hook 'js-mode-hook (lambda ()  ( prettier-js-mode t)))


(setq centaur-package-archives 'ustc)         ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'night)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
(setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
(setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode  web-mode)) ; Ignore format on save for some languages
(setq centaur-chinese-calendar t)              ; Use Chinese calendar or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq centaur-prettify-org-symbols-alist t)  ; Alist of symbol prettifications for `org-mode'

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



;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list
   '("/Volumes/D/shide/Intelligent-ransportation-waring-frontend/.agignore"))
 '(all-the-icons-color-icons t)
 '(centaur-completion-style 'childframe)
 '(centaur-package-archives 'melpa)
 '(company-async-redisplay-delay 0)
 '(company-backends
   '((company-capf :with company-yasnippet :with company-capf)
     (company-dabbrev-code company-keywords company-files)
     company-dabbrev))
 '(company-begin-commands '(self-insert-command))
 '(company-box-doc-delay 0.2)
 '(company-box-enable-icon nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-tabnine-max-restart-count 40)
 '(company-tabnine-show-annotation t)
 '(company-tabnine-wait 0.1)
 '(company-tooltip-idle-delay 0)
 '(doom-modeline-mode t)
 '(eglot-send-changes-idle-time 0.0)
 '(fci-rule-color "#a3a1a1")
 '(font-lock-maximum-size 50000000)
 '(font-lock-support-mode 'jit-lock-mode)
 '(frame-resize-pixelwise t)
 '(gc-cons-percentage 0.5)
 '(jit-lock-stealth-time 16)
 '(js-jsx-indent-level 2)
 '(line-spacing 2)
 '(lsp-ui-doc-delay 0.13)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-show-with-cursor t)
 '(lsp-ui-doc-use-webkit nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(objed-cursor-color "#c82829")
 '(pdf-view-midnight-colors (cons "#4d4d4c" "#ffffff") t)
 '(scroll-conservatively 0)
 '(scroll-step 0)
 '(typescript-indent-level 2)
 '(warning-minimum-level :error)
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(cfrs-border-color ((t (:background "#9ca0a4"))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#51afef" :background nil))))
 '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
 '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
 '(flycheck-posframe-face ((t (:foreground "#718c00"))))
 '(flycheck-posframe-info-face ((t (:foreground "#718c00"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(hl-todo ((t (:inherit variable-pitch :box (:line-width -1) :height 0.85 :width condensed :weight semibold :underline nil :inverse-video t))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(ivy-posframe ((t (:inherit tooltip))))
 '(ivy-posframe-border ((t (:background "#a5a4a5"))))
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
 '(transient-posframe ((t (:inherit tooltip))))
 '(transient-posframe-border ((t (:background "#a5a4a5"))))
 '(which-key-posframe ((t (:inherit tooltip))))
 '(which-key-posframe-border ((t (:background "#a5a4a5"))))
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
