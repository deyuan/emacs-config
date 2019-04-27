;; Deyuan Notes
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
;; <Tab> : indent-for-tab-command : fix indentation of region
;; M-x find-file-at-point : vim's gf
;; M-x chmod : chmod
;; M-x dirs : refresh current directory in shell
;; M-x eval-buffer, M-x load-file : reload .emacs
;; M-x whitespace-mode : show whitespaces
;; M-x list-packages : list available packages
;; M-x package-install : install a package

(package-initialize)

;; Use MELPA package repository
(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/") t)

;; Common
(progn
  ;; Hide startup screen
  (setq inhibit-startup-screen t)
  ;; Hide tool bar
  (tool-bar-mode -1)
  ;; Put backup files in a central place
  (setq backup-directory-alist `(("." . "~/.emacs.save")))
  ;; Require new line at the end of file
  (setq require-final-newline t)
  ;; Unset C-z, C-x f
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x f") nil)
  ;; Use y/n instead of yes/no
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Reload files if changed on disk
  (global-auto-revert-mode t)
  ;; Parentheses
  (show-paren-mode t)
  ;; Type to overwrite highlighted region
  (delete-selection-mode 1)
  ;; Scroll smoothly
  (setq scroll-conservatively 200)
  (setq scroll-margin 5)
  ;; Tabs: width and expand
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  ;; Whitespace: show tabs and trailing spaces in C++
  (setq whitespace-style '(face tabs trailing tab-mark))
  (add-hook 'c++-mode-hook (lambda () (whitespace-mode t)))
  ;; Shell: make prompts read only
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  )

;; Theme
(progn
  (require 'dracula-theme)
  (load-theme 'dracula t)
  )

;; Window numbering: M-1/2/3
(progn
  (require 'window-numbering)
  (custom-set-faces '(window-numbering-face ((t (:foreground "cyan1" :weight bold)))))
  (window-numbering-mode t)
  )

;; Autocomplete
(progn
  (require 'icomplete)
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (icomplete-mode 1)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company window-numbering dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
