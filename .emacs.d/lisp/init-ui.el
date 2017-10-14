;; ------------------------------------------------------------------------------
;; Filename: init-ui.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp UI configuration module
;;
;; ------------------------------------------------------------------------------


;; UI settings (these are a bit of a mess right now... not gonna lie)
;; Sane Defaults.
;; I realize the danger.
(setq auto-save-default nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

;; I realize the danger.
(setq ring-bell-function 'ignore)

(blink-cursor-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ns-pop-up-frames nil)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-attribute 'default nil :background "#32302f" :foreground "#ebebeb")
(set-face-attribute 'fringe nil :background "#32302f")
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#736075")
(set-face-attribute 'font-lock-comment-face nil :foreground "#736075")
(set-face-attribute 'show-paren-match nil :foreground "#ccff66")
(set-face-attribute 'show-paren-mismatch nil :foreground "#ff2f92")

;; Selection Highlighting
(set-face-attribute 'region nil :background "#ccff66" :foreground "#333333")

(setq column-number-mode t)
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist '(cursor-color . "ffffff"))
(global-hl-line-mode t)
(set-face-background hl-line-face "#393642")
(set-frame-font "Inconsolata-12" nil t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq linum-format "  %d ") ; Space out gutter.
(server-start)

;; Tabs = spaces * 4
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq tab-stop-list (number-sequence 8 120 8))

;; Cleanup trailing whitespace, et al after write
(add-hook 'after-save-hook 'whitespace-cleanup)

;; Smooth scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-preserve-screen-position t
      scroll-step 1)

;; Window Transparency (#active, #inactive)
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

;; Keybindings
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

(use-package linum-relative
  :ensure t
  :config
  (global-linum-mode t)
  (linum-relative-mode t)
  (setq linum-relative-current-symbol ">")
  ;; Space out gutter.
  (setq linum-relative-format " %3s "))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(provide 'init-ui)
