;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Tested with Emacs Version 28
;; Deyuan Guo, 2024
;;; Code:

;;------------------------------------------------------------------------------
;; Initialize use-package
;;------------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;------------------------------------------------------------------------------
;; Configure Emacs
;;------------------------------------------------------------------------------

(use-package emacs
  :config
  ;; startup
  (setq inhibit-startup-screen t)
  ;; Adjust garbage collection thresholds during startup, and thereafter
  (let ((normal-gc-cons-threshold (* 20 1024 1024))
        (init-gc-cons-threshold (* 128 1024 1024)))
    (setq gc-cons-threshold init-gc-cons-threshold)
    (add-hook 'emacs-startup-hook
              (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))
    )
  ;; Process performance tuning
  (setq read-process-output-max (* 4 1024 1024))
  (setq process-adaptive-read-buffering nil)
  ;; GUI
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;(when window-system (set-frame-size (selected-frame) 240 60))  ; default size
  ;(setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
  ;; mouse
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't)  ; scroll window under mouse
  (setq mouse-yank-at-point t)
  ;; scroll
  (setq scroll-conservatively 200)
  (setq scroll-margin 5)
  (dolist (hook '(term-mode-hook shell-mode-hook))
    (add-hook hook (lambda()
                     (make-local-variable 'scroll-margin)
                     (setq scroll-margin 1))))
  ;; prompt
  (defalias 'yes-or-no-p 'y-or-n-p)  ; use y/n instead of yes/no
  ;; backup
  (setq make-backup-files nil)
  (setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
  ;; files
  (global-auto-revert-mode 1)  ; auto refreh if files are changed on disk
  (save-place-mode 1)  ; save last location
  ;; mode line
  (size-indication-mode 1)  ; show file size in mode line
  (setq column-number-mode t)  ; show column number in mode line
  ;; buffers
  (setq-default indicate-empty-lines t)
  ;(setq-default truncate-lines t)
  ;(setq truncate-partial-width-windows nil) ;; for vertically-split windows
  ;; copy/paste
  (setq save-interprogram-paste-before-kill t)
  ;(setq x-select-enable-clipboard-manager nil)
  ;; edit
  (delete-selection-mode 1)  ; type to overrite highlighted region
  (setq-default tab-width 4)  ; this is buffer-local so use setq-default
  (setq-default indent-tabs-mode nil)  ; don't use tab to indent
  ;; bookmark
  (setq-default bookmark-default-file (expand-file-name "bookmarks.el" user-emacs-directory))
  ;; ediff
  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; search
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
  ;; shell
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  ;(add-hook 'term-mode-hook
  ;          (lambda() (local-set-key [S-<mouse-2>] #'term-paste)))
  ;;(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)  ; open in current window
  ;; setup programming modes
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook (lambda()
                     (setq c-default-style "linux")
                     (setq c-basic-offset 2)
                     ;(setq tcl-indent-level 2)
                     (electric-pair-mode t)
                     (show-paren-mode t)
                     (setq whitespace-style '(face tabs tab-mark))
                     (whitespace-mode t)
                     (linum-mode t)
                     ))
    )
  ;; open .h file in C++ mode by default
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )

(use-package winner
  :config
  (winner-mode t)
  )

(use-package windmove
  :config
  (windmove-default-keybindings 'meta)  ; Meta + Arrow
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "<M-right>") nil)
              (define-key org-mode-map (kbd "<M-left>") nil)
              (define-key org-mode-map (kbd "<M-up>") nil)
              (define-key org-mode-map (kbd "<M-down>") nil)
              ))
  )

;;------------------------------------------------------------------------------
;; Configure More Packages
;;------------------------------------------------------------------------------

;(use-package all-the-icons
;  :ensure t
;  :if (display-graphic-p)
;  )

(use-package neotree
;  :after all-the-icons
  :ensure t
  :config
  ;(setq neo-theme (if (display-graphic-p) 'arrow))
  (setq neo-theme 'ascii)
  )

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)  ; enable flashing mode-line on errors
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-lighter "")
  (which-key-mode)
  )

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle 1)
  :init
  (vertico-mode)
  )

(use-package savehist
  :init
  (savehist-mode)
  )

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode)  ; show number of search results
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (setq anzu-search-threshold 500)
  )

(use-package winum
  :ensure t
  :config
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          (define-key map (kbd "M-9") 'winum-select-window-9)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          map))
  ;(set-face-attribute 'winum-face nil :weight 'bold)
  (winum-mode)  ; Meta-1/2/3
  )

(use-package ace-jump-mode
  :ensure t
  )

(use-package multi-term
  :ensure t
  )


(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)  ; ace-jump
  (evil-set-undo-system 'undo-redo)  ; support redo
  (evil-mode 1)
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd ";") 'evil-ex))  ; map : to ;
  (global-set-key [remap evil-quit] 'kill-buffer-and-window)  ; :q close buffer instead of entire emacs
  (evil-ex-define-cmd "q" 'kill-this-buffer)  ; :q close buffer but not window
  (evil-ex-define-cmd "wq" (lambda()
                             (interactive)
                             (save-buffer)
                             (kill-this-buffer)
                             ))  ; :wq close buffer but not window
  (setq-default evil-cross-lines t)  ; horizontal move can cross lines
  )

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (setq evil-collection-mode-list nil)
  (add-to-list 'evil-collection-mode-list '(term term ansi-term multi-term))
  (add-to-list 'evil-collection-mode-list 'ediff)
  (add-to-list 'evil-collection-mode-list 'dired)
  :config
  (evil-collection-init)
  )

(use-package bind-key
  :ensure t
  :config
  (bind-key* "<f9>" 'ff-find-other-file nil)
  )

;;------------------------------------------------------------------------------
;; DEV
;;------------------------------------------------------------------------------

;(use-package tex
;  :ensure auctex
;  :config
  ;(setq Tex-auto-save t)
  ;(setq Tex-parse-self t)
  ;(setq-default Tex-master nil)
;  (setq tex-dvi-view-command "xdvi")
;  )

;(use-package magit
;  :ensure t
;  )

;(use-package p4
;  :ensure t
;  )

(use-package json-mode
  :ensure t
  )

;(use-package xcscope
;  :ensure t
;  :config
;  (setq cscope-initial-directory (getenv "xref"))
;  )

(use-package flycheck
  :ensure t
  ;:init
  ;(global-flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 2.0)
  (setq flycheck-idle-buffer-switch-delay 2.0)
  :hook (c++-mode . (lambda()
                      (setq flycheck-clang-language-standard "c++-17")
                      ))
  )

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 2.0)
  )

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)  ; which-key integration
         )
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-idle-delay 2.0)
  :commands lsp
  )

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  )

;;------------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;------------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;------------------------------------------------------------------------------
;; Interactive Functions
;;------------------------------------------------------------------------------

(defun dg-show-trailing-whitespace ()
  "Show trailing whitespaces."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  )

(defun dg-open-init-el ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

;;; init.el ends here

