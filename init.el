;; Notes
;; C-w, M-w, C-y : cut, copy, paste
;; C-g : keyboard-quit : quit any operation
;; C-q <Tab> : quote-insert : inset a tab character
;; C-x C-q : read-only-mode : edit read-only file
;; C-M-f, C-M-b : forward-sexp, backward-sexp : goto matching parentheses
;; C-backspace : backward-kill-word
;; C-0 C-k : kill line backwards
;; C-u M-x shell : multiple shells
;; C-x +/-/0 : adjust font size
;; M-g-g : goto-line
;; M-f, M-b, M-backspace, M-d : word navigation and deletion
;; M-<, M-> : goto head/tail
;; M-% : query-replace : Use C-q C-j to replace with newline
;; <Tab> : indent-for-tab-command : fix indentation of region
;; M-x find-file-at-point : vim's gf
;; M-x chmod : chmod
;; M-x dirs, M-RET : refresh current directory in shell
;; M-x eval-buffer, M-x load-file : reload .emacs
;; M-x whitespace-mode : show whitespaces
;; M-x list-packages : list available packages
;; M-x package-install : install a package
;; M-x ff-find-other-file : switch between source and header
;; M-x find-tag : ctags
;; M-x occur : show occurrence
;; M-x list-colors-display : list all colors

(package-initialize)

;; Use MELPA package repository
(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/") t)

;; Display and Theme
(progn
  ;; Frame
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)
  ;; (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode)
  (setq frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
  ;; Theme
  (require 'dracula-theme)
  (load-theme 'dracula t)
  ;; Highlight active mode line
  (set-face-attribute 'mode-line nil :background "purple4")
  (set-face-attribute 'mode-line-inactive nil :background "gray25")
  (set-face-attribute 'window-divider nil :foreground "gray60")
  )

;; Common
(progn
  ;; Put backup files in a central place
  (setq backup-directory-alist `(("." . "~/.emacs.save")))
  ;; Unset C-z, C-x f
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x f") nil)
  (global-set-key (kbd "C-x C-b") nil)
  (global-set-key (kbd "C-x C-k") nil)
  (global-set-key (kbd "<help>") nil)
  ;; Use y/n instead of yes/no
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Scroll smoothly
  (setq scroll-conservatively 200)
  (setq scroll-margin 5)
  (add-hook 'shell-mode-hook (lambda () (make-local-variable 'scroll-margin) (setq scroll-margin 0)))
  ;; (setq scroll-preserve-screen-position 'always)
  ;; Mouse and touch pad
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-yank-at-point t)
  ;; Reload files if changed on disk
  (global-auto-revert-mode t)
  ;; Show column number
  (setq column-number-mode t)
  ;; Show end of buffer
  (setq-default indicate-empty-lines t)
  ;; Parentheses
  (show-paren-mode t)
  ;; Type to overwrite highlighted region
  (delete-selection-mode 1)
  ;; Shell: make prompts read only
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  ;; Move focus to help window
  (setq help-window-select t)
  ;; Highlight current cursor line
  ;;(global-hl-line-mode t)
  (set-cursor-color "green")
  )

;; Programming
(progn
  ;; Tabs: width and expand
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  ;; Fix c/c++ indentation
  (setq c-default-style "linux")
  (setq c-basic-offset 2)
  )

;; Whitespace: show tabs and trailing spaces in some modes
(progn
  (require 'whitespace)
  (setq whitespace-style '(face tabs trailing tab-mark))
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook (lambda () (whitespace-mode t))))
  )

;; Winner mode: C-c + Left/Right
(progn
  (require 'winner)
  (winner-mode 1)
  )

;; Window numbering: M-1/2/3
(progn
  (require 'window-numbering)
  ;; (custom-set-faces '(window-numbering-face ((t (:foreground "cyan1" :weight bold)))))
  (window-numbering-mode t)
  )

;; Window move: Meta + Arrow
(progn
  (require 'windmove)
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

;; Autocomplete
(progn
  (require 'icomplete)
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (icomplete-mode 1)
  )

;; Search
(progn
  (require 'isearch)
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
  (require 'anzu)
  (global-anzu-mode)
  (setq anzu-mode-lighter "")
  )

;; Which Key
(progn
  (require 'which-key)
  (setq which-key-idle-delay 2.0)
  (setq which-key-lighter "")
  (which-key-mode)
  )

;; Smex
(progn
  (require 'smex)
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex)
  )

;; IDO for buffer switching only
(progn
  (require 'ido)
  (setq ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory))
  ;; ido find file may have performance issue
  ;; (ido-mode 1)
  (ido-mode 'buffers)
  (setq ido-enable-flex-matching t)
  (setq ido-max-prospects 100)
  ;; Disable tab pop up buffer
  (setq ido-completion-buffer nil)
  )

;; Ace Jump
(progn
  (require 'ace-jump-mode)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (global-set-key (kbd "<f8>") 'ace-jump-mode)
  )

;; Magit
(progn
  (require 'magit)
  )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove ^M characters
(defun my-rm-ctrl-m ()
  "Remove ^M characters"
  (interactive)
  (beginning-of-buffer)
  (replace-string "" "")
  )


