;; defaults 
;;------------------------------------------------------------------------------ 
;; turn it off!
(blink-cursor-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files -1)

(setq inhibit-startup-message t) 
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(global-hl-line-mode 1)
(add-to-list 'default-frame-alist '(font . "Menlo-11"))
(setq-default fill-column 80)
(server-start)
(setq scroll-preserve-screen-position 1)

;; plugin management
;;------------------------------------------------------------------------------ 
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))

(package-initialize)

;; figure this out later...
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))
;;(eval-when-compile
;;  (require 'use-package))

;; plugin settings
;;------------------------------------------------------------------------------ 
(require 'goto-last-change)

;; figure this out later...
;;(use-package magit
;;  :ensure t)

;; evil 
(require 'evil-leader)
(require 'evil)
(require 'evil-surround)
(global-evil-leader-mode) ;; must come before setting evil
(evil-mode t)
(global-evil-tabs-mode t)
(global-evil-surround-mode 1)
(evil-set-initial-state 'dire-mode 'normal) ;; enter insert mode to edit a commit message

;; functions and macros
;;------------------------------------------------------------------------------ 
;; functions

;; macros
;; insert newline.. recorded macro (hacky.. not good)
(fset 'insert_line
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
(setq key-chord-two-keys-delay 0.25)
(key-chord-mode 1)

;; regular
;; (global-set-key (kbd "C-z") 'display_sleep) ;; buggy
;; (key-chord-define-global "gs" 'other-window)

;; evil
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state) ;; remap escape-to-normal

(define-key evil-normal-state-map (kbd ";") 'evil-ex) ;; remap : to ;

;; evil leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "`" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "0" (kbd "@q") 
  "2" (kbd "@@") 
  "@" 'evil-execute-macro
  "-" 'evil-scroll-page-up
  "=" 'evil-scroll-page-down

  "t" 'elscreen-create
  "T" 'dired
  "i" 'insert_line

  "s" 'other-window
  "S" 'split-window-below
  "f" 'find-file
  "j" (lambda () (interactive) (evil-next-line 10))
  "k" (lambda () (interactive) (evil-previous-line 10))
  "l" 'goto-last-change

  "b" 'list-buffers)

;; aesthetics 
;;------------------------------------------------------------------------------
;; theme
(when window-system
  (load-theme 'gruvbox t))

;; parenthesis stuff
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; cursor & cursorline
(set-face-background hl-line-face "#3c3836")
(set-cursor-color "#f1958c")

;; selection highlighting
(set-face-attribute 'region nil :background "#3c3836")

;; colors, etc. 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mismatch ((t (:box (:line-width 2 :color "#fb4934" :style released-button)))))
 '(show-paren-match ((t (:background "#fabd2f")))))
