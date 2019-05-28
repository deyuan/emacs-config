;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; A self-contained single-file emacs configuration
;; Author: Deyuan Guo
;; Reference: https://github.com/purcell/emacs.d
;;; Code:

;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------
(progn
  ;; Enable error backtrace
  ;;(setq debug-on-error t)
  ;; Minimum emacs version
  (let ((minver "25.1"))
    (when (version< emacs-version minver)
      (error "This config requires emacs v%s or higher" minver)))
  ;; Custom file location
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  )

;; Initialize packages
(progn
  (require 'package)
  ;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
  (let ((versioned-package-dir
         (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                           user-emacs-directory)))
    (setq package-user-dir versioned-package-dir))
  ;; MELPA
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  ;; Install required packages if needed
  (require 'seq)
  (let ((my-packages '(color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized
                       which-key smex anzu winum mode-line-bell beacon
                       ace-jump-mode yasnippet magit yaml-mode vlf expand-region
                       evil evil-anzu)))
    (let ((missing-packages (seq-remove 'package-installed-p my-packages)))
      (when missing-packages
        (package-refresh-contents)
        (mapc #'package-install missing-packages))))
  )

;;------------------------------------------------------------------------------
;; Theme
;;------------------------------------------------------------------------------
;; Ensure that themes will be applied even if they have not been customized
(defun sanityinc/reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun my-light-theme ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (sanityinc/reapply-themes))

(defun my-dark-theme ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (sanityinc/reapply-themes))

(progn
  ;; If you don't customize it, this is the theme you get.
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
  ;; Load theme now to avoid flashing
  (load-theme 'sanityinc-tomorrow-bright t)
  (add-hook 'after-init-hook 'sanityinc/reapply-themes)
  )

;;------------------------------------------------------------------------------
;; GUI Frames
;;------------------------------------------------------------------------------
(progn
  (setq inhibit-startup-screen t)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (if (eq system-type 'gnu/linux) (menu-bar-mode -1))
  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border))
  ;; Divider
  (setq window-divider-default-right-width 1)
  (window-divider-mode)
  ;; Frame title
  (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
  )

;;------------------------------------------------------------------------------
;; Windows
;;------------------------------------------------------------------------------
(defun my-split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(defun my-split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun my-split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(progn
  ;; Winner mode: C-c + Left/Right
  (winner-mode 1)
  ;; When splitting window, show (other-buffer) in the new window
  (global-set-key (kbd "C-x 2") (my-split-window-func-with-other-buffer 'split-window-vertically))
  (global-set-key (kbd "C-x 3") (my-split-window-func-with-other-buffer 'split-window-horizontally))
  ;; Toggle horizontally or vertically split windows
  (global-set-key (kbd "C-x |") 'my-split-window-horizontally-instead)
  (global-set-key (kbd "C-x _") 'my-split-window-vertically-instead)
  ;; Move focus to help window
  (setq help-window-select t)
  )

;; Window move: Meta + Arrow
(progn
  (windmove-default-keybindings 'meta)
  ;; (setq org-replace-disputed-keys t)
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "<M-right>") nil)
              (define-key org-mode-map (kbd "<M-left>") nil)
              (define-key org-mode-map (kbd "<M-up>") nil)
              (define-key org-mode-map (kbd "<M-down>") nil)
              ))
  )

;; Winum
(progn
  ;; Winum: M-1/2/3
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  (require 'winum)
  (set-face-attribute 'winum-face nil :weight 'bold)
  (winum-mode)
  )

;;------------------------------------------------------------------------------
;; Search
;;------------------------------------------------------------------------------
(progn
  ;; Search backspace behavior
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
  ;; show number of matches while searching and replacing
  (require 'anzu)
  (global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  )

;;------------------------------------------------------------------------------
;; Editing Utils
;;------------------------------------------------------------------------------
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(progn
  ;; Electric pair mode
  (electric-pair-mode t)
  ;; Cursor blink
  (setq-default blink-cursor-interval 0.4)
  ;; Bookmark file
  (setq-default bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory))
  ;; Show column number
  (setq column-number-mode t)
  ;; Type to overwrite highlighted region
  (delete-selection-mode 1)
  ;; Ediff
  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Tabs: width and expand
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  ;; Backup file location
  (setq make-backup-files nil)
  (setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
  ;; Scroll smoothly
  (setq scroll-conservatively 200)
  (setq scroll-margin 5)
  (add-hook 'shell-mode-hook (lambda () (make-local-variable 'scroll-margin) (setq scroll-margin 0)))
  ;; Mouse and touch pad
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-yank-at-point t)
  ;; Clipboard
  (setq save-interprogram-paste-before-kill t)
  ;; Reload files if changed on disk
  (global-auto-revert-mode 1)
  ;; Show end of buffer
  (setq-default indicate-empty-lines t)
  ;; Parentheses
  (show-paren-mode t)
  ;; Expand region
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  ;; Shift + Enter
  (global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)
  ;; Fix c/c++ indentation
  (setq c-default-style "linux")
  (setq c-basic-offset 2)
  ;; Whitespace: show tabs and trailing spaces in some modes
  (setq whitespace-style '(face tabs trailing tab-mark))
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook (lambda () (whitespace-mode t))))
  ;; Ace jump mode
  (require 'ace-jump-mode)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (global-set-key (kbd "<f8>") 'ace-jump-mode)
  )

;; Unset keys
(progn
  (global-set-key (kbd "C-z") nil) ; suspend-frame
  (global-set-key (kbd "C-x f") nil) ; set-fill-column
  (global-set-key (kbd "C-x C-b") nil) ; list-buffers
  (global-set-key (kbd "C-x C-k") nil) ; kmacro
  (global-set-key (kbd "C-x C-u") nil) ; upcase-region
  (global-set-key (kbd "C-x C-l") nil) ; downcase-region
  (global-set-key (kbd "<help>") nil)
  )

;; Very large file
(require 'vlf)
(defun my-ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;; Yasnippet
(progn
  (require 'yasnippet)
  (setq yas-snippet-dirs `("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  ;; Use fundamental mode snippets everywhere
  (add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))
  ;; Use IDO for yas-insert-snippet
  (setq yas-prompt-functions '(yas-ido-prompt))
  ;; Key shortcut
  (global-set-key (kbd "<f7>") 'yas-insert-snippet)
  ;; Don't use TAB
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  )

;; Evil
(progn
  ;; Use default emacs keybindings in insert state
  (setq evil-disable-insert-state-bindings t)
  (require 'evil)
  ;; Default state
  (setq evil-default-state 'emacs)
  (evil-mode 1)
  ;; Normal state j/k based on visual line
  (define-key evil-normal-state-map "j" 'evil-next-visual-line)
  (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
  ;; Disable :q and :wq
  (evil-ex-define-cmd "q" nil)
  (evil-ex-define-cmd "wq" nil)
  ;; Config cursor colors and shapes
  ;(setq evil-emacs-state-cursor '("green" box))
  ;(setq evil-normal-state-cursor '("cyan" box))
  ;(setq evil-visual-state-cursor '("cyan" box))
  ;(setq evil-insert-state-cursor '("cyan" bar))
  ;(setq evil-replace-state-cursor '("cyan" hbar))
  ;(setq evil-operator-state-cursor '("cyan" evil-half-cursor))
  ;; Show search count with n/N in evil nornal mode
  (require 'evil-anzu)
  )

;;------------------------------------------------------------------------------
;; Completion
;;------------------------------------------------------------------------------
;; Icomplete
(progn
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (icomplete-mode 1)
  )

;; IDO
(progn
  ;; IDO for buffer switching only
  (ido-mode 'buffers)
  (setq ido-enable-flex-matching t)
  (setq ido-max-prospects 100)
  ;; Disable tab pop up buffer
  (setq ido-completion-buffer nil)
  )

;; Which key
(progn
  (require 'which-key)
  (setq which-key-idle-delay 1.0)
  (setq which-key-lighter "")
  (which-key-mode)
  )

;; Smex
(progn
  (require 'smex)
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex)
  )

;;------------------------------------------------------------------------------
;; Version Control
;;------------------------------------------------------------------------------
(progn
  (require 'magit)
  )

;;------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------
(progn
  ;; Use y/n instead of yes/no
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Shell: make prompts read only
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  )

;; Mode line bell
(progn
  (require 'mode-line-bell)
  (add-hook 'after-init-hook 'mode-line-bell-mode)
  )

;; Beacon: Highlight cursor location
(progn
  (require 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (add-hook 'after-init-hook 'beacon-mode)
  )

;;------------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;------------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;------------------------------------------------------------------------------
;; My Elisp Utilities
;;------------------------------------------------------------------------------

;; Aliases for multiple shells
(defun my-switch-to-or-create-shell (s)
  "Switch to or create a shell with buffer name s"
  (if (get-buffer s) (switch-to-buffer s) (shell s)))
(defun shell2 ()
  "Switch to or create *shell*<2>"
  (interactive)
  (my-switch-to-or-create-shell "*shell*<2>"))
(defun shell3 ()
  "Switch to or create *shell*<3>"
  (interactive)
  (my-switch-to-or-create-shell "*shell*<3>"))
(defun shell4 ()
  "Switch to or create *shell*<4>"
  (interactive)
  (my-switch-to-or-create-shell "*shell*<4>"))
(defun shell5 ()
  "Switch to or create *shell*<5>"
  (interactive)
  (my-switch-to-or-create-shell "*shell*<5>"))

;; Remove ^M characters
(defun my-rm-ctrl-m ()
  "Remove ^M characters"
  (interactive)
  (beginning-of-buffer)
  (replace-string "" "")
  )


