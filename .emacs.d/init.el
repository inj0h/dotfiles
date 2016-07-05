;; plugin management
;;------------------------------------------------------------------------------ 
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; (sane) defaults 
;;------------------------------------------------------------------------------ 
(setq inhibit-startup-message t) 
(setq-default indent-tabs-mode nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq column-number-mode t)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(add-to-list 'default-frame-alist '(font . "Menlo-11"))
(setq-default fill-column 80)
(setq scroll-preserve-screen-position 1)

;; backup files 
(setq make-backup-files -1)
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; aesthetics 
;;------------------------------------------------------------------------------
;; theme
(when window-system
  (load-theme 'gruvbox t))

(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background hl-line-face "gray13")
(global-evil-tabs-mode t)

;; plugin settings
;;------------------------------------------------------------------------------ 
(require 'goto-last-change)
(global-set-key [(meta p)(u)] 'goto-last-change)

;; evil 
;;------------------------------------------------------------------------------
(require 'evil-leader)
(require 'evil)
(require 'evil-surround)
(global-evil-leader-mode) ;; must come before setting evil
(evil-mode t)
(global-evil-surround-mode 1)

;; functions and macros
;;------------------------------------------------------------------------------ 
;; functions

;; macros
;; insert newline.. recorded macro (very hacky)
(fset 'place_line
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([48 105 return escape 107] 0 "%d")) arg)))

(defun display_sleep()
  (interactive)
  (compile "~/.bin/sh/display_sleep.sh"))

;; keybindings
;;------------------------------------------------------------------------------
;; keychord stuff
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

;; regular
;; (global-set-key (kbd "C-z") 'display_sleep) ;; buggy
(key-chord-define-global "gs" 'other-window)

;; evil
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state) ;; remap escape-to-normal

(define-key evil-normal-state-map (kbd ";") 'evil-ex) ;; remap : to ;

;; evil leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "i" 'place_line)

;; open init.el
(evil-leader/set-key "`" (lambda () (interactive)
                           (find-file "~/.emacs.d/init.el")))

(evil-leader/set-key "0" 'evil-scroll-line-to-center)
(evil-leader/set-key "-" 'evil-scroll-page-up)
(evil-leader/set-key "=" 'evil-scroll-page-down)

(evil-leader/set-key "s" 'split-window-below)
(evil-leader/set-key "l" 'goto-last-change)


