;; startup 
;;------------------------------------------------------------------------------ 
(setq inhibit-startup-message t)                                                ;; disable splash screen 
(tool-bar-mode -1)                                                              ;; disable toolbar
(scroll-bar-mode -1)                                                            ;; disable scrollbar
(setq column-number-mode t)                                                     ;; show column number
(global-hl-line-mode 1)                                                         ;; highlight current line
(blink-cursor-mode 0)                                                           ;; stop blinking cursor
(add-to-list 'default-frame-alist '(font . "Menlo-11"))                         ;; set font

;; backup files 
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))                   ;; store backups in .emacs

;; package manager
;;------------------------------------------------------------------------------ 
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))



(setq package-enable-at-startup nil)
(package-initialize)

;; aesthetics 
;;------------------------------------------------------------------------------
(when window-system
  (load-theme 'gruvbox t))


(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background hl-line-face "gray13")

;; evil 
;;------------------------------------------------------------------------------ 
(require 'evil)
(evil-mode t)

;; keybindings
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
