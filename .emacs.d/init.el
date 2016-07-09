;; defaults
;;------------------------------------------------------------------------------
;; shut it off!
;;---------------------------------------
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files -1
      inhibit-startup-screen t)

;; personal info
;;---------------------------------------
(setq user-full-name "erikoelrojo"
      user-mail-address "eric.chung2718@gmail.com")

;; all's well with these
;;---------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(global-hl-line-mode 1)
(add-to-list 'default-frame-alist '(font . "Menlo-11"))
(setq-default fill-column 80)
(server-start)
(setq scroll-preserve-screen-position 1)

;; windowing
;;---------------------------------------
;; always split vertically (one on top of the other)
(setq split-height-threshold nil
      split-width-threshold 0)

;; tabs and whitespace
;;---------------------------------------
;; no tab chars please
(setq-default indent-tabs-mode nil)

;; tabs = spaces * 4
(setq c-basic-offset 4)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

(setq show-trailing-whitespace t)

;; plugin management
;;------------------------------------------------------------------------------
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

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
(require 'evil-leader) ;; <- must come before setting evil?
(global-evil-leader-mode) ;; <- must come before setting evil
(require 'evil)
(require 'evil-mc)
(require 'evil-surround)
(evil-mode t)
(global-evil-mc-mode  1)
(global-evil-tabs-mode t)
(global-evil-surround-mode 1)

;; utility functions
;;------------------------------------------------------------------------------
(defun display_sleep()
  (interactive)
  (compile "~/.bin/sh/display_sleep.sh"))

(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun cleanup-whitespace ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

;; keyboard macros
;;------------------------------------------------------------------------------
;; insert newline
(fset 'insert_line
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([48 105 return escape 107] 0 "%d")) arg)))


;; keybindings
;;------------------------------------------------------------------------------
;; keychord
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

;; global (emacs)
;;---------------------------------------
;; (global-set-key (kbd "C-z") 'display_sleep) ;; buggy
;; (key-chord-define-global "gs" 'other-window)
;; (define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)

;; evil!
;;---------------------------------------
;; escape everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state) ;; remap escape-to-normal

;; ergonomic!
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'execute-extended-command)

;; evil leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "`" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "2" (kbd "@@")
  "@" (kbd "@q")
  "-" 'evil-scroll-page-up
  "=" 'evil-scroll-page-down

  "t" 'elscreen-create
  "T" 'dired
  "i" 'insert_line
  "I" 'indent-whole-buffer

  "s" 'other-window
  "S" 'split-window-below
  "f" 'find-file
  "g" 'keyboard-quit
  "j" (lambda () (interactive) (evil-next-line 10))
  "k" (lambda () (interactive) (evil-previous-line 10))
  "l" 'goto-last-change

  "c" 'cleanup-whitespace
  "C" 'comment-dwim
  "b" 'list-buffers
  "n" 'count-words-region)

;; more evil! (there must be a better way to do this! find out!)
;; evil everywhere!.. <- not sure if this works
;; (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
;; (setq evil-motion-state-modes nil)
;; (setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
;; (setq evil-emacs-state-modes nil)

;; dired
(define-key dired-mode-map "$" 'evil-end-of-line)
(define-key dired-mode-map "0" 'evil-beginning-of-line)
(define-key dired-mode-map "w" 'evil-forward-word-begin)
(define-key dired-mode-map "f" 'evil-find-char)
(define-key dired-mode-map "g" 'evil-goto-first-line)
(define-key dired-mode-map "G" 'evil-goto-line)
(define-key dired-mode-map "h" 'evil-backward-char)
(define-key dired-mode-map "j" 'evil-next-line)
(define-key dired-mode-map "k" 'evil-previous-line)
(define-key dired-mode-map "l" 'evil-forward-char)
(define-key dired-mode-map "b" 'evil-backward-word-begin)
(define-key dired-mode-map  "/" 'evil-search-forward)

;; package menu
(define-key package-menu-mode-map "$" 'evil-end-of-line)
(define-key package-menu-mode-map "0" 'evil-beginning-of-line)
(define-key package-menu-mode-map "w" 'evil-forward-word-begin)
(define-key package-menu-mode-map "f" 'evil-find-char)
(define-key package-menu-mode-map "g" 'evil-goto-first-line)
(define-key package-menu-mode-map "G" 'evil-goto-line)
(define-key package-menu-mode-map "h" 'evil-backward-char)
(define-key package-menu-mode-map "j" 'evil-next-line)
(define-key package-menu-mode-map "k" 'evil-previous-line)
(define-key package-menu-mode-map "l" 'evil-forward-char)
(define-key package-menu-mode-map "b" 'evil-backward-word-begin)
(define-key package-menu-mode-map  "/" 'evil-search-forward)

;; aesthetics
;;------------------------------------------------------------------------------
;; theme
(when window-system
  (load-theme 'gruvbox t))

;; parenthesis
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; cursor & cursorline
(set-cursor-color "#ff6666")
(set-face-background hl-line-face "#3c3836")

;; selection highlighting
(set-face-attribute 'region nil :background "#daffb3")

;; colors, etc.
;;---------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mismatch ((t (:box (:line-width 2 :color "#fb4934" :style released-button)))))
 '(show-paren-match ((t (:background "#fabd2f")))))
