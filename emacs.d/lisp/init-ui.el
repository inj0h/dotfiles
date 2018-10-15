;; Filename: init-ui.el
;; Maintainer: erikorojo
;; License: n/a
;; Comments: Elisp UI configuration module
;;

;; UI settings (these are a bit of a mess right now... not gonna lie)
;; Sane Defaults.
;; I realize the danger.
(setq auto-save-default nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

;; I realize the danger.
(setq ring-bell-function 'ignore)

;; Colors
(setq my-color-black-charcoal "#333333"
      my-color-gray-gengar "#393642"
      my-color-gray-slate "#32302f"
      my-color-green-betelgeuse "#ccff66"
      my-color-pink-palahniuk "#ff2f92"
      my-color-violet-lemony "#736075"
      my-color-white-apple "#ffffff"
      my-color-white-ivory "#ebebeb")

(blink-cursor-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ns-pop-up-frames nil)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-attribute 'default nil
                    :background my-color-gray-slate
                    :foreground my-color-white-ivory)
(set-face-attribute 'fringe nil :background my-color-gray-slate)
(set-face-attribute 'font-lock-comment-delimiter-face nil
                    :foreground my-color-violet-lemony)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground my-color-violet-lemony)
(set-face-attribute 'show-paren-match nil
                    :foreground my-color-green-betelgeuse)
(set-face-attribute 'show-paren-mismatch nil
                    :foreground my-color-pink-palahniuk)

;; Window management.
(add-hook 'find-file-hook 'delete-other-windows)

;; Selection Highlighting
(set-face-attribute 'region nil
                    :background my-color-green-betelgeuse
                    :foreground my-color-black-charcoal)

(setq column-number-mode t)
(add-to-list 'default-frame-alist '(cursor-color . "#ff2f92"))
(global-hl-line-mode t)
(set-face-background hl-line-face my-color-gray-gengar)
(set-frame-font "Inconsolata-16" nil t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq linum-format "  %d ") ; Space out gutter.

;; Tabs = spaces * 4
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq tab-stop-list (number-sequence 8 120 8))
(setq tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

;; Cleanup trailing whitespace, et al after write
(add-hook 'after-save-hook 'whitespace-cleanup)

;; Column indicator

(defun my-fci-settings ()
  "Use fci for these modes."
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)
  (add-hook 'sh-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode))

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
  (setq linum-relative-current-symbol ""))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package fill-column-indicator
  :ensure t
  :config
  (my-fci-settings)
  (setq fci-rule-column 79)
  (setq column-number-indicator-zero-based nil))

(provide 'init-ui)
