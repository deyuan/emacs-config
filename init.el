;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; A self-contained single-file emacs configuration
;; Author: Deyuan Guo
;; - Interactive functions use prefix my-
;; - Internal functions use prefix sanityinc/
;; Reference:
;; - https://github.com/purcell/emacs.d
;; - https://github.com/redguardtoo/emacs.d
;;; Code:

;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------

(progn
  ;;(setq debug-on-error t)
  ;; Mininum version required
  (let ((minver "25.1"))
    (when (version< emacs-version minver)
      (error "This config requires emacs v%s or higher" minver)))
  )

(defun sanityinc/install-packages-if-needed (my-packages)
  "Install packages if needed. Don't refresh if all packages are installed."
  (let ((missing-packages (seq-remove 'package-installed-p my-packages)))
    (when missing-packages
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(progn
  ;; Custom file location
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Each emacs version uses a separate directory
  (require 'package)
  (let ((versioned-package-dir
         (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                           user-emacs-directory)))
    (setq package-user-dir versioned-package-dir))
  ;; Add melpa
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  ;; Install required packages if needed
  (require 'seq)
  (let ((my-packages '(color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized
                       which-key smex anzu winum mode-line-bell beacon bind-key
                       ace-jump-mode yasnippet magit yaml-mode vlf expand-region
                       )))
    (sanityinc/install-packages-if-needed my-packages))
  )

;;------------------------------------------------------------------------------
;; Theme
;;------------------------------------------------------------------------------

(defun sanityinc/reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun my-day-theme ()
  "Activate a day color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (sanityinc/reapply-themes))
(defun my-night-theme ()
  "Activate a night color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (sanityinc/reapply-themes))
(defun my-light-theme ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-light))
  (sanityinc/reapply-themes))
(defun my-dark-theme ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-dark))
  (sanityinc/reapply-themes))

(progn
  ;; Default theme
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
  ;; Load theme now to avoid screen flashing
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
  ;; Border
  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border))
  ;; Divider
  (setq window-divider-default-right-width 1)
  (window-divider-mode)
  ;; Frame title
  (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
  ;; Default font
  (if (eq system-type 'gnu/linux) (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12")))
  (if (eq system-type 'darwin) (add-to-list 'default-frame-alist '(font . "Menlo-14")))
  ;; Show file size in mode line
  (size-indication-mode 1)
  )

;;------------------------------------------------------------------------------
;; Windows
;;------------------------------------------------------------------------------

(defun sanityinc/split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(defun sanityinc/split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun sanityinc/split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(progn
  ;; When splitting window, show (other-buffer) in the new window
  (global-set-key (kbd "C-x 2") (sanityinc/split-window-func-with-other-buffer 'split-window-vertically))
  (global-set-key (kbd "C-x 3") (sanityinc/split-window-func-with-other-buffer 'split-window-horizontally))
  ;; Toggle horizontally or vertically split windows
  (global-set-key (kbd "C-x |") 'sanityinc/split-window-horizontally-instead)
  (global-set-key (kbd "C-x _") 'sanityinc/split-window-vertically-instead)
  ;; Move focus to help window
  (setq help-window-select t)
  ;; Winner mode: C-c + Left/Right
  (winner-mode 1)
  )

(progn
  ;; Window move: Meta + Arrow
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
  ;; Show number of matches while searching and replacing
  (require 'anzu)
  (global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (setq anzu-search-threshold 500)
  )

;;------------------------------------------------------------------------------
;; Editing Utils
;;------------------------------------------------------------------------------

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
  ;; Fix c/c++ indentation
  (setq c-default-style "linux")
  (setq c-basic-offset 2)
  ;; Whitespace: show tabs and trailing spaces in some modes
  (setq whitespace-style '(face tabs trailing tab-mark))
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook (lambda () (whitespace-mode t))))
  )

(progn
  ;; Expand region
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  ;; Ace jump mode
  (require 'ace-jump-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
  (setq ace-jump-mode-move-keys
        (loop for i from ?a to ?z collect i))
  )

(progn
  ;; Very large file
  (require 'vlf)
  )

(defun my-ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

(progn
  ;; Yasnippet
  (require 'yasnippet)
  (setq yas-snippet-dirs `("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  ;; Use fundamental mode snippets everywhere
  (add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))
  ;; Use IDO for yas-insert-snippet
  (setq yas-prompt-functions '(yas-ido-prompt))
  ;; Don't use TAB
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  )

;;------------------------------------------------------------------------------
;; Completion
;;------------------------------------------------------------------------------

(progn
  ;; Icomplete
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (icomplete-mode 1)
  )

(progn
  ;; IDO for buffer switching only
  (ido-mode 'buffers)
  (setq ido-enable-flex-matching t)
  (setq ido-max-prospects 100)
  ;; Disable tab pop up buffer
  (setq ido-completion-buffer nil)
  )

(progn
  (require 'which-key)
  (setq which-key-idle-delay 0.5)
  (setq which-key-lighter "")
  (which-key-mode)
  )

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
  )

(progn
  ;; Shell: Make prompts read only
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  ;; Shell: Open in current window
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
  )

(progn
  (require 'mode-line-bell)
  (add-hook 'after-init-hook 'mode-line-bell-mode)
  )

(progn
  ;; Highlight cursor location
  (require 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (add-hook 'after-init-hook 'beacon-mode)
  )

;;------------------------------------------------------------------------------
;; Key Bindings
;;------------------------------------------------------------------------------

(progn
  ;; Unset keys
  (global-set-key (kbd "C-z") nil) ; suspend-frame
  (global-set-key (kbd "C-x f") nil) ; set-fill-column
  (global-set-key (kbd "C-x C-b") nil) ; list-buffers
  (global-set-key (kbd "C-x C-k") nil) ; kmacro
  (global-set-key (kbd "C-x C-u") nil) ; upcase-region
  (global-set-key (kbd "C-x C-l") nil) ; downcase-region
  (global-set-key (kbd "C-x C-SPC") nil) ; pop-global-mark
  (global-set-key (kbd "<help>") nil)
  )

(defun sanityinc/open-shell (s)
  "Switch to or create a shell with buffer name s"
  (if (get-buffer s) (switch-to-buffer s) (shell s)))

(defun sanityinc/bind-key (key func &optional desc)
  "Bind a key sequence to func, with a which-key description string"
  (bind-key* key func)
  (which-key-add-key-based-replacements key desc)
  )

(defun sanityinc/bind-to-lead-key (key func &optional desc)
  "Bind a lead key sequence to func, with a which-key description string"
  (let ((key-seq (concat "C-j " key))) (sanityinc/bind-key key-seq func desc))
  (let ((key-seq (concat "<f8> " key))) (sanityinc/bind-key key-seq func desc))
  )

(progn
  ;; Key Bindings
  (require 'bind-key)
  ;; Shift + Enter
  (bind-key* "S-<return>" (lambda() (interactive) (move-end-of-line 1) (newline-and-indent)) "open-line")
  ;; Yasnippet
  (sanityinc/bind-to-lead-key "y" 'yas-insert-snippet nil)
  ;; Rectangle
  (sanityinc/bind-to-lead-key "r" 'rectangle-mark-mode nil)
  (sanityinc/bind-to-lead-key "t" 'string-rectangle nil)
  ;; Shell
  (sanityinc/bind-to-lead-key "1" (lambda() (interactive) (sanityinc/open-shell "*shell*<1>")) "shell-1")
  (sanityinc/bind-to-lead-key "2" (lambda() (interactive) (sanityinc/open-shell "*shell*<2>")) "shell-2")
  (sanityinc/bind-to-lead-key "3" (lambda() (interactive) (sanityinc/open-shell "*shell*<3>")) "shell-3")
  (sanityinc/bind-to-lead-key "4" (lambda() (interactive) (sanityinc/open-shell "*shell*<4>")) "shell-4")
  (sanityinc/bind-to-lead-key "5" (lambda() (interactive) (sanityinc/open-shell "*shell*<5>")) "shell-5")
  ;; Remove ^M characters
  (sanityinc/bind-to-lead-key "m" (lambda() (interactive) (beginning-of-buffer) (replace-string "\r" "")) "remove-^M")
  )

;;------------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;------------------------------------------------------------------------------

(when (file-exists-p custom-file)
  (load custom-file))


