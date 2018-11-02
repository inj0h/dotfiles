;; Filename: init.el
;; Maintainer: erikorojo
;; License: n/a
;; Comments: Elisp configuration file
;;

;; Manage Lisp files and packages
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Warning!
;; At the time of writing this comment, use-package and its dependencies,
;; bind-key and diminish, might break when downloading from Melpa.
;;
;; If that problem comes up, just use Melpa stable to install them.
;;

;; Load local-only settings file if it exists on disk, and don't throw a warning
;; if it doesn't.
(load "~/dotfiles/emacs.d/local-settings" 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Dump all the custom-var-face s*** here.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;
;; Start UI configuration
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
  (add-hook 'css-mode-hook 'fci-mode)
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)
  (add-hook 'haskell-mode-hook 'fci-mode)
  (add-hook 'html-mode-hook 'fci-mode)
  (add-hook 'js-mode-hook 'fci-mode)
  (add-hook 'python-mode-hook 'fci-mode)
  (add-hook 'scss-mode-hook 'fci-mode)
  (add-hook 'sh-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode)
  (add-hook 'web-mode-hook 'fci-mode))

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

(use-package which-key
  :ensure t
  :config
  (setq which-key-mode t))

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

;;
;; End UI configuration
;;
;; Start Evil configuration
;;

(defun my-evil-settings ()
  "Vi keybindings, etc."
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)
    (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
    (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
    (define-key evil-motion-state-map (kbd "s-e") 'eval-last-sexp)
    (define-key evil-insert-state-map (kbd "C-y") 'yas-insert-snippet)))

(defun my-toggle-linenumbers ()
  "Toggle between regular/relative/no line numbers. Assume Emacs defaults to no
   line numbers."
  (interactive)
  (cond ((and (not (bound-and-true-p linum-mode))
              (not (bound-and-true-p linum-relative-mode)))
         (linum-mode))
        ((and (bound-and-true-p linum-mode)
              (not (bound-and-true-p linum-relative-mode)))
         (linum-relative-mode))
        ((and (bound-and-true-p linum-mode)
              (bound-and-true-p linum-relative-mode))
         (setq linum-relative-mode nil)
         (linum-relative-off))))

(defun my-evil-leader-settings ()
  "Configure evil leader-based keybindings."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "`" 'delete-other-windows
    "2" (kbd "@@")
    "pa" 'projectile-add-known-project
    "ps" 'projectile-switch-project
    "f" 'helm-do-ag-buffers
    "/" 'whitespace-mode
    "l" 'goto-last-change
    "L" 'my-toggle-linenumbers
    "ohh" 'helm-projectile-find-file
    "oho" 'helm-projectile-find-other-file
    "ohw" 'helm-projectile-switch-to-buffer
    "oo" 'helm-find-files
    "or" 'helm-recentf
    "ow" 'helm-buffers-list
    "s" 'other-window
    "S" 'split-window-below
    "ka" 'which-key-show-keymap
    "kma" 'which-key-show-major-mode
    "kmi" 'which-key-show-minor-mode-keymap
    "kq" 'which-key-abort
    "ms" 'magit-status
    "mbp" 'magit-blame-popup
    "mbq" 'magit-blame-quit))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-in-single-undo t)
  (setq evil-undo-list-pointer t)
  (setq evil-want-fine-undo t)
  (my-evil-settings)

  (use-package evil-escape
    :ensure t
    :config
    (evil-escape-mode t)
    (setq-default evil-escape-key-sequence "hh")
    (setq-default evil-escape-delay 0.25))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (my-evil-leader-settings))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode)))

;;
;; End Evil configuration
;;
;; Start Search and Completion configuration
;;

(defun my-helm-settings ()
  "General helm settings."
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-idle-delay 0.0)
  (setq helm-input-idle-delay 0.01)
  (setq helm-autoresize-max-height 60)
  (helm-autoresize-mode 1)

  ;;(defadvice helm-display-mode-line (after undisplay-header activate)
  ;; (setq header-line-format nil))
  )

(defun my-helm-keybindings ()
  "Helm keybindings, etc."
  (define-key helm-map [tab] 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-o") 'helm-select-action))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode +1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (my-helm-settings)
  (my-helm-keybindings)

  (use-package helm-projectile
    :ensure t
    :config
    ))

;;
;; End Search and Completion configuration
;;
;; Start Language configuration
;;

;; Support Human Languages... or at least English.
(cond ((string-equal system-type "gnu/linux")
       (setq ispell-program-name "/usr/bin/aspell"))
      ((string-equal system-type "darwin")
       (setq ispell-program-name "/usr/local/bin/aspell")))

(defun my-rainbow-delimeters-settings ()
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'html-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'javascript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scss-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode))

(defun yas-enable-and-reload ()
  "Enable yas-minor-mode for buffer and reload all snips"
  (yas-minor-mode)
  (yas-reload-all))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook '(lambda() (set-fill-column 80)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

;; Colored delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (my-rainbow-delimeters-settings))

;; Flycheck linter
(use-package flycheck
  :ensure t
  :config
  (add-hook 'text-mode-hook 'flyspell-mode))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ;; Not sure if this hook does much atm.
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "/usr/local/bin/pandoc")
  (add-hook 'markdown-mode-hook 'flycheck-mode))

(use-package web-mode
  :ensure t
  :config
  )

;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  )

;;
;; End Language configuration
;;
