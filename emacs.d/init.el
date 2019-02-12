;; Filename:    init.el
;; Description: Main Emacs Lisp configuration file.
;;

;; Manage Lisp files and packages
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load local-only settings file if it exists on disk, and don't throw a warning
;; if it doesn't.
(load "~/dotfiles/emacs.d/settings-local-load-pre" 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Before getting the other things (at least for now).
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Dump all the custom-var-face s*** here.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;
;; Start UI configuration
;;
;; UI settings (these are a bit of a mess right now... not gonna lie)
;; Sane Defaults.
;; I realize the danger.
(setq auto-save-default nil
      inhibit-startup-screen t
      make-backup-files nil)

;; I realize the danger.
(setq ring-bell-function 'ignore)

(blink-cursor-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq frame-title-format nil
      ns-pop-up-frames nil
      ns-use-proxy-icon nil
      show-paren-delay 0)
(show-paren-mode 1)

;; Window management.
(add-hook 'find-file-hook 'delete-other-windows)

(setq column-number-mode t)
(setq-default fill-column 80)
(global-hl-line-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq linum-format "  %d ") ; Space out gutter.

;; Font stuff
(if (eq system-type 'gnu/linux)
    (set-frame-font "Inconsolata-16" nil t)
  (set-frame-font "Menlo-15" nil t))

;; Tabs = spaces * 4
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

;; Cleanup trailing whitespace, et al after write
(add-hook 'after-save-hook 'whitespace-cleanup)

;; Column indicator

(defun my-fci-settings ()
  "Use fci for these modes."
  (add-hook 'css-mode-hook        'fci-mode)
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)
  (add-hook 'haskell-mode-hook    'fci-mode)
  (add-hook 'html-mode-hook       'fci-mode)
  (add-hook 'js-mode-hook         'fci-mode)
  (add-hook 'python-mode-hook     'fci-mode)
  (add-hook 'scss-mode-hook       'fci-mode)
  (add-hook 'sh-mode-hook         'fci-mode)
  (add-hook 'text-mode-hook       'fci-mode)
  (add-hook 'web-mode-hook        'fci-mode))

;; Smooth scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-preserve-screen-position t
      scroll-step 1)

;; Window Transparency (#active, #inactive)
(set-frame-parameter (selected-frame) 'alpha '(100 . 90))
(add-to-list 'default-frame-alist '(alpha . (100 . 90)))

;; Keybindings
;; General
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2)
  (setq which-key-sort-order 'which-key-key-order-alpha))

(use-package abyss-theme
  :ensure t
  :config
  (load-theme 'abyss t))

(use-package fill-column-indicator
  :ensure t
  :config
  (my-fci-settings)
  (setq fci-rule-column 79) ; Because fci counts starting at 0. Ugh.
  (setq column-number-indicator-zero-based nil))

;;
;; End UI configuration
;;
;; Start Evil configuration
;;

(defun my-evil-settings ()
  "Vi keybindings, etc."
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";")   'evil-ex)
    (define-key evil-motion-state-map (kbd ":")   'evil-repeat-find-char)
    (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
    (define-key evil-insert-state-map (kbd "C-y") 'yas-insert-snippet)))

(defun my-evil-leader-settings ()
  "Configure evil leader-based keybindings."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "`"   'delete-other-windows
    "2"   (kbd "@@")
    "pa"  'projectile-add-known-project
    "ps"  'projectile-switch-project
    "gb"  'magit-branch-popup
    "glc" 'magit-blame-copy-hash
    "glp" 'magit-blame-popup
    "glq" 'magit-blame-quit
    "gpa" 'magit-pull-from-upstream
    "gpb" 'magit-push-current-to-upstream
    "gs"  'magit-status
    "lc"  'count-words-region
    "lk"  'counsel-yank-pop
    "ls"  'sort-lines
    "lw"  'whitespace-mode
    "L"   'linum-mode
    "ol"  'evil-switch-to-windows-last-buffer
    "on"  'counsel-find-file
    "oo"  'ivy-switch-buffer
    "ow"  'counsel-projectile-find-file
    "dn"  'woman
    "sg"  'counsel-rg
    "sl"  'goto-last-change
    "sr"  'query-replace
    "ka"  'which-key-show-keymap
    "kk"  'which-key-abort
    "kma" 'which-key-show-major-mode
    "kmi" 'which-key-show-minor-mode-keymap
    "w"   'other-window))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
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
    (evil-commentary-mode))

  (use-package evil-magit
    :ensure t
    :config
    ))

;;
;; End Evil configuration
;;
;; Start Search and Completion configuration
;;
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :bind
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("M-x"   . counsel-M-x))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package projectile
  :ensure t
  :init (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)

  (use-package counsel-projectile
    :ensure t
    :after projectile))

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

(defun my-rainbow-delimeters-hooks ()
  (add-hook 'css-mode-hook        #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook    #'rainbow-delimiters-mode)
  (add-hook 'html-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook         #'rainbow-delimiters-mode)
  (add-hook 'scss-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'sh-mode-hook         #'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook        #'rainbow-delimiters-mode))

(defun yas-enable-and-reload ()
  "Enable yas-minor-mode for buffer and reload all snips"
  (yas-minor-mode)
  (yas-reload-all))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook '(lambda () (set-fill-column 80)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (local-set-key (kbd "s-e") 'eval-last-sexp)))

;; Markdown
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'markdown-mode-hook 'turn-off-fci-mode)

;; Text
(add-hook 'text-mode-hook '(lambda () (set-fill-column 80)))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Colored delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (my-rainbow-delimeters-hooks))

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

(use-package gitignore-mode :ensure t)

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

;; Load local-only settings file after reading the main init file, i.e. useful
;; when you need to override variables, etc.
;;
;; Don't throw a warning if it doesn't exist on disk.
(load "~/dotfiles/emacs.d/settings-local-load-post" 1)
