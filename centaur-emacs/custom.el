;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)



(add-hook 'prog-mode-hook (lambda()
                            (setq scroll-step 0)
                            (setq scroll-conservatively 0)

                            (global-hl-line-mode  -1)
                            (global-subword-mode -1)
                            (global-hungry-delete-mode -1)
                            (highlight-indent-guides-mode -1)
                            (diff-hl-flydiff-mode -1)
                            (global-diff-hl-mode -1)
                            (flyspell-mode -1)
                            (magit-todos-mode -1)
                            (display-line-numbers-mode -1)

                            (persistent-scratch-mode -1)
                            (persistent-scratch-autosave-mode -1)
                            (global-company-mode -1)
                            ))

(setq ns-alternate-modifier 'super)

(setq ns-command-modifier 'meta)

(setq centaur-icon t)
(setq centaur-full-name "angelporo")              ; User full name
(setq centaur-mail-address "940079461@qq.com")    ; Email address
(setq centaur-proxy "127.0.0.1:1086")             ; Network proxy
;; Enable proxy
(proxy-http-enable)

(setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil

(setq frame-resize-pixelwise t)

(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'random)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
(setq centaur-completion-style 'childframe)    ; Completion display style: minibuffer or childframe
(setq centaur-dashboard t)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-restore-frame-geometry t)      ; Restore the frame's geometry at startup: t or nil
(setq centaur-lsp nil)                   ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode )) ; Ignore format on save for some languages
(setq centaur-tree-sitter t)                 ; Enable `tree-sitter' or not: t or nil
(setq centaur-chinese-calendar nil)              ; Use Chinese calendar or not: t or nil
(setq centaur-prettify-symbols-alist t)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))
                                                                                                                             ;; Misc.
(setq confirm-kill-emacs 'y-or-n-p)


;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 )


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
