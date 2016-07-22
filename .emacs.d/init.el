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
(setq user-full-name "erikoelrojo")

;; Strong Defaults
;;---------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)
(global-hl-line-mode 1)
(add-to-list 'default-frame-alist '(font . "Menlo-11"))
(setq-default fill-column 80)
(server-start)
(setq ispell-program-name "/usr/local/bin/aspell")

;; Smooth scrolling.
;;---------------------------------------
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-preserve-screen-position t
      scroll-step 1)

;; Dired
;;---------------------------------------
;; Auto-refresh Dired upon file change(s)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Tabs and Whitespace
;;---------------------------------------
;; No tab chars please.
(setq-default indent-tabs-mode nil)

;; Tabs = spaces * 4
(setq c-basic-offset 4
      tab-width 4
      tab-stop-list (number-sequence 4 120 4))

;; Windowing
;;---------------------------------------
;; Always split vertically (one on top of the other). ;; <- Fix!
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

;; Fix!
;; Disable Evil Tab top-left-most indent.
;; (defadvice elscreen-tab-control-face (after undisplay-header activate)
;;   (setq header-line-format nil))

;; Helm
;;---------------------------------------
(require 'helm)
(require 'helm-config)

;; Disable Helm help bar and
(defadvice helm-display-mode-line (after undisplay-header activate)
  (setq header-line-format nil))

;; Fuzzy finding.
(setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)

;; Update fast sources immediately.
(setq helm-idle-delay 0.0
      helm-input-idle-delay 0.01) ;; <- This actually updates things.

;; Windowing.
(helm-autoresize-mode 1)
;; (setq helm-autoresize-max-height 33)

(helm-mode 1)

;; Utility Functions
;;------------------------------------------------------------------------------
(defun display_sleep ()
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
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

;; Global (Emacs Keys)
;;---------------------------------------
;; Navigation
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "s-q") 'keyboard-quit)

;; Tabs and Windows
(global-set-key (kbd "s-1") 'delete-other-windows)

(global-set-key (kbd "s-}") 'evil-tabs-goto-tab) ; Like MacOS
(global-set-key (kbd "s-{") 'elscreen-previous) ; Like MacOS

(global-set-key (kbd "s-x") 'delete-window)

;; ..Et al
(global-set-key (kbd "s-Z") 'display_sleep)

;; Evil Keys 
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
(define-key evil-normal-state-map (kbd ";") 'execute-extended-command)

;; Multiple Cursors
(define-key evil-normal-state-map (kbd "C-m") 'evil-mc-make-all-cursors)
(define-key evil-normal-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
;; (define-key evil-mc-key-map (kbd "jj") 'evil-mc-undo-all-cursors)
;; (define-key evil-mcnormal-state-map (kbd "C-q") 'evil-mc-undo-all-cursors)
;; (define-key evil-normal-state-map (kbd "C-p") 'evil-mc-make-and-goto-prev-match)

;; Evil Leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "`"  (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "2"  (kbd "@@")
  "4"  'async-shell-command
  "-"  'evil-scroll-page-up
  "="  'evil-scroll-page-down

  "w1" 'delete-other-windows
  "wd" 'delete-window
  "el" 'eval-last-sexp
  "t"  'elscreen-create
  "i"  'insert_line
  "I"  'indent-whole-buffer
  "p"  'flycheck-mode ; <- p = parse
  "["  'pop-tag-mark
  "]"  (lambda () (interactive) (find-tag (find-tag-default-as-regexp)))
  "\\" 'list-tags

  "a0" (lambda () (interactive) (flyspell-mode 0)) ; <- a = aspell
  "a1" (lambda () (interactive) (flyspell-mode 1))
  "aa" 'flyspell-buffer
  "ap" 'flyspell-prog-mode
  "@"  (kbd "@q")
  "s"  'other-window
  "S"  'split-window-below
  "f"  'helm-find-files
  "j"  (lambda () (interactive) (evil-next-line 10))
  "k"  (lambda () (interactive) (evil-previous-line 10))
  "l"  'goto-last-change

  "c"  'comment-dwim
  "C"  'cleanup-whitespace
  "b"  'helm-buffers-list
  "n"  'count-words-region

  "/"  'show-trailing-whitespace)

;; Evil (Almost) Everywhere.
(defun more-evil ()
  "Extend Evilness to 'Good' modes."
  (local-set-key (kbd "j") 'next-line)
  (local-set-key (kbd "k") 'previous-line)
  (local-set-key (kbd "/") 'evil-search-forward)
  (local-set-key (kbd "n") 'evil-search-next))

(add-hook 'dired-mode-hook 'more-evil)
(add-hook 'package-menu-mode-hook 'more-evil)

;; Fix! 
;; (dolist (mode '(dired-mode
;;                 package-menu-mode))
;;   (add-to-list 'evil-emacs-state-modes mode))

;; Helm Keys 
;;---------------------------------------
(define-key helm-map [tab] 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") 'helm-find-files-down-last-level)
(define-key helm-map (kbd "C-k") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-o") 'helm-select-action) ; <- o = option

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

;; ..Et al
;;---------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-control-face ((t (:background "#282828" :foreground "#282828" :box nil :underline nil))))
 '(mode-line ((t (:box nil :foreground "#b8bb26" :background "#504945"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#fabd2f"))))
 '(show-paren-match ((t (:background "#fabd2f"))))
 '(show-paren-mismatch ((t (:box (:line-width 2 :color "#fb4934" :style released-button))))))
