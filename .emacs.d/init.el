;; Filename: init.el
;; Description: Emacs configuration file.
;; -----------------------------------------------------------------------------

;; Defaults
;;------------------------------------------------------------------------------
;; Shut It Off!
;;---------------------------------------
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; I realize the danger.
(setq auto-save-default nil
      inhibit-startup-screen t
      make-backup-files nil)

;; Personal Info
;;---------------------------------------
(setq user-full-name "erikoelrojo"
      user-mail-address "eric.chung2718@gmail.com")

;; Strong Defaults
;;---------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(global-hl-line-mode 1)
(add-to-list 'default-frame-alist '(font . "Menlo-11"))
(setq-default fill-column 80)
(server-start)
(setq scroll-preserve-screen-position 1)
(setq ispell-program-name "/usr/local/bin/aspell")

;; Dired
;;---------------------------------------
;; Auto-refresh Dired upon file change(s)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Tabs and Whitespace
;;---------------------------------------
;; No tab chars please.
(setq-default indent-tabs-mode nil)

;; Tabs = spaces * 4
(setq c-basic-offset 4)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

;; Windowing
;;---------------------------------------
;; Always split vertically (one on top of the other). ;; <- Doesn't work yet!
;; (setq split-height-threshold nil
;;       split-width-threshold 0)

;; Package Management
;;------------------------------------------------------------------------------
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; Package Settings
;;------------------------------------------------------------------------------
;; Get my packages!
(load "~/.emacs.d/load-packages.el")

;; "exec-path-from-shell"
;;---------------------------------------
;; Use the shell environment on MacOS
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Evil
;;---------------------------------------
(require 'evil-leader) ;; <- Must come before setting evil?
(require 'evil)
(require 'evil-mc)
(require 'evil-surround)
(global-evil-leader-mode) ;; <- Must come before setting evil?
(evil-mode 1)
(global-evil-mc-mode 1)
(global-evil-tabs-mode t)
(global-evil-surround-mode 1)

;; Helm
;;---------------------------------------
(require 'helm)
(require 'helm-config)

;; Helm windowing.
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 33)
(setq helm-autoresize-min-height 33)
(helm-mode 1)

;; Utility Functions
;;------------------------------------------------------------------------------
(defun display_sleep()
  (interactive)
  (compile "~/.bin/sh/display_sleep.sh"))

(defun cleanup-whitespace ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Keyboard Macros
;;------------------------------------------------------------------------------
;; Insert a newline.
(fset 'insert_line
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([48 105 return escape 107] 0 "%d")) arg)))

;; Keybindings
;;------------------------------------------------------------------------------
;; Keychord
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

;; Global (Emacs Mode)
;;---------------------------------------
;; (global-set-key (kbd "C-z") 'display_sleep) ;; buggy
;; (key-chord-define-global "gs" 'other-window)
;; (define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Evil Mode
;;---------------------------------------
;; Evil Global
;; Escape everything.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (Ergonomic!) Normal Mode
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state) ;; remap escape-to-normal
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'execute-extended-command)

;; Multiple Cursors
(define-key evil-normal-state-map (kbd "C-m") 'evil-mc-make-all-cursors)
(define-key evil-normal-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
(define-key evil-normal-state-map (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
(define-key evil-normal-state-map (kbd "C-q") 'evil-mc-undo-all-cursors)

;; Evil Leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "`" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "2" (kbd "@@")
  "4" 'shell-command
  "-" 'evil-scroll-page-up
  "=" 'evil-scroll-page-down

  "el" 'eval-last-sexp
  "t" 'elscreen-create
  "T" 'dired
  "i" 'insert_line
  "I" 'indent-whole-buffer
  "[" 'pop-tag-mark
  "]" (lambda () (interactive) (find-tag (find-tag-default-as-regexp)))
  "\\" 'list-tags

  "a0" (lambda () (interactive) (flyspell-mode 0))
  "aa" 'flyspell-buffer
  "af" (lambda () (interactive) (flyspell-mode 1))
  "ap" 'flyspell-prog-mode
  "@" (kbd "@q")
  "s" 'other-window
  "S" 'split-window-below
  "f" 'helm-find-files
  "j" (lambda () (interactive) (evil-next-line 10))
  "k" (lambda () (interactive) (evil-previous-line 10))
  "l" 'goto-last-change

  "c" 'cleanup-whitespace
  "C" 'comment-dwim
  "b" 'list-buffers
  "n" 'count-words-region

  "/" 'show-trailing-whitespace)

;; More evil! (there must be a better way to do this! find out!)
;; evil everywhere!.. <- not sure if this works
;; (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
;; (setq evil-motion-state-modes nil)
;; (setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
;; (setq evil-emacs-state-modes nil)

;; Dired
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

;; Package Menu
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

;; Aesthetics
;;------------------------------------------------------------------------------
;; Background
(set-face-attribute 'default t :background "#32302f")
(set-face-attribute 'fringe t :background "#32302f")

;; Cursor and Cursorline
(set-cursor-color "#ff6666")
(add-to-list 'default-frame-alist '(cursor-color . "ff6666"))
(set-face-background hl-line-face "#3c3836")

;; Parenthesis
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Selection Highlighting
(set-face-attribute 'region nil :background "#daffb3")

;; Theme
(when window-system
  (load-theme 'gruvbox t))

;; Window Transparency (#active, #inactive)
(set-frame-parameter (selected-frame) 'alpha '(97 . 78))
(add-to-list 'default-frame-alist '(alpha . (97 . 78)))

;; Et al
;;---------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "#fabd2f"))))
 '(show-paren-mismatch ((t (:box (:line-width 2 :color "#fb4934" :style released-button))))))
