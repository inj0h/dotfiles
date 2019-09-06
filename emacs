;; Filename: emacs
;; Note:     Main Emacs Lisp configuration file.
;;

;; Bugfix for v26.2 (apparently).
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Manage Lisp files and packages
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load local-only settings file if it exists on disk, and don't throw a warning
;; if it doesn't.
(load "~/dotfiles/emacs.d/local-preload" 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Before getting the other things (at least for now).
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Mac load path shenanigans.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (string-equal system-type "darwin")
    (exec-path-from-shell-initialize)))

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
(global-hl-line-mode -1)
(setq frame-title-format nil
      ns-pop-up-frames nil
      ns-use-proxy-icon nil
      show-paren-delay 0)
(show-paren-mode 1)

;; Window management.
(add-hook 'find-file-hook 'delete-other-windows)

(setq column-number-mode t
      my/default-column-limit 80)
(setq-default column-number-indicator-zero-based nil)
(setq-default fill-column my/default-column-limit)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq display-line-numbers-grow-only t)

;; Font stuff
(if (eq system-type 'gnu/linux)
    (set-frame-font "Inconsolata-17" nil t)
  (set-frame-font "Menlo-14" nil t))

;; Tabs = spaces * 4
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

;; Cleanup trailing whitespace, et al after write
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Smooth scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-preserve-screen-position t
      scroll-step 1)

;; Window Transparency (#active, #inactive)
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

;; Minibuffer settings
(add-hook 'minibuffer-setup-hook '(lambda () (setq truncate-lines nil)))

;; Keybindings
;; General

;; Scale text.
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-=") '(lambda () (interactive) (text-scale-adjust 0)))

;; Super yank-pop
(global-set-key (kbd "s-p") 'yank-pop)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2)
  (setq which-key-sort-order 'which-key-key-order-alpha))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

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
    (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)))

(defun my-evil-leader-settings ()
  "Configure evil leader-based keybindings."
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "2"   (kbd "@@")

    "gb"  'magit-branch
    "glb" 'magit-blame
    "glc" 'magit-blame-copy-hash
    "glg" 'magit-show-commit
    "glq" 'magit-blame-quit
    "gs"  'magit-status
    "gul" 'magit-pull-from-upstream
    "guu" 'magit-push-current-to-upstream

    "lc"  'count-words-region
    "ll"  'display-line-numbers-mode
    "ls"  'sort-lines
    "lw"  'whitespace-mode

    "pa"  'projectile-add-known-project
    "pr"  'projectile-remove-known-project
    "ps"  'projectile-switch-project
    "pn"  'projectile-find-file

    "oL"  'find-file-literally-at-point
    "oN"  'find-file-at-point
    "oT"  'ido-switch-buffer-other-window
    "oU"  'bookmark-set
    "oc"  'delete-other-windows
    "oh"  'other-window
    "ol"  'find-file-literally
    "on"  'ido-find-file
    "or"  'evil-switch-to-windows-last-buffer
    "ot"  'ido-switch-buffer
    "ou"  'bookmark-bmenu-list

    "n,"  'replace-string
    "n."  'replace-regexp
    "na"  'me/kill-filepath
    "ne"  'query-replace
    "no"  'goto-last-change
    "nu"  'deadgrep

    "ka"  'which-key-show-keymap
    "kk"  'which-key-abort
    "kma" 'which-key-show-major-mode
    "kmi" 'which-key-show-minor-mode-keymap

    "sa"  'me/add-word-to-dictionary)

  (evil-leader/set-key-for-mode 'deadgrep-mode
    "nu"  'deadgrep-visit-result-other-window)

  (evil-leader/set-key-for-mode 'org-mode
    ",co" 'outline-hide-other
    ",cr" 'outline-hide-subtree
    ",d"  'org-demote-subtree
    ",ld" 'org-toggle-link-display
    ",lg" 'browse-url
    ",p"  'org-promote-subtree
    ",se" 'org-sort-entries
    ",sl" 'org-sort-list
    ",ss" 'org-sort
    ",st" 'org-table-sort-lines
    ",t"  'org-todo))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (my-evil-settings)

  (use-package evil-escape
    :ensure t
    :config
    (evil-escape-mode t)
    (setq-default evil-escape-key-sequence "hh"
                  evil-escape-excluded-states '(normal visual emacs motion)
                  evil-escape-delay 0.2))

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
    (add-hook 'magit-status-mode-hook
              '(lambda () (setq magit-diff-refine-hunk t)))))

;;
;; End Evil configuration
;;
;; Start Search and Completion configuration
;;

(setq ido-enable-flex-matching t
      ido-case-fold t
      ido-everywhere t)
(ido-mode 1)

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package deadgrep
  :ensure t
  :config
  (with-eval-after-load 'deadgrep
    (evil-define-key 'normal
      deadgrep-mode-map (kbd "q") 'kill-buffer-and-window)))

(use-package projectile
  :ensure t
  :init (setq projectile-completion-system 'ido)
  :config
  (projectile-mode +1))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq-default company-dabbrev-downcase nil)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

;;
;; End Search and Completion configuration
;;
;; Start Language and Mode configuration
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
  (add-hook 'java-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook         #'rainbow-delimiters-mode)
  (add-hook 'scss-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'sh-mode-hook         #'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook        #'rainbow-delimiters-mode))

;; Text
(add-hook 'text-mode-hook
          '(lambda () (set-fill-column my/default-column-limit)))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Colored delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (my-rainbow-delimeters-hooks))

(defun me/add-word-to-dictionary ()
  "Add the word-at-point to aspell's dictionary."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save
                           nil
                           (car word)
                           current-location
                           (cadr word)
                           (caddr word)
                           current-location))))

;; Flycheck linter
(use-package flycheck
  :ensure t
  :config
  (add-hook 'text-mode-hook 'flyspell-mode))

;; Emacs Lisp
(add-to-list 'auto-mode-alist '("\\emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))

(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (set-fill-column my/default-column-limit)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (local-set-key (kbd "s-e") 'eval-last-sexp)))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ;; Not sure if this hook does much atm.
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Git
(use-package git-commit
  :ensure t
  :config
  (setq git-commit-summary-max-length 50
        my/git-commit-mode-column-limit 72)

  (add-hook 'git-commit-mode-hook
            '(lambda ()
               (setq-local whitespace-line-column
                           my/git-commit-mode-column-limit)))
  (add-hook 'git-commit-mode-hook
            '(lambda () (set-fill-column my/git-commit-mode-column-limit)))
  (add-hook 'git-commit-mode-hook '(lambda () (turn-on-auto-fill))))

(use-package gitignore-mode :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (cond ((string-equal system-type "gnu/linux")
         (setq markdown-command "/usr/bin/pandoc"))
        ((string-equal system-type "darwin")
         (setq markdown-command "/usr/local/bin/pandoc")))

  (add-hook 'markdown-mode-hook '(lambda () (setq-local truncate-lines t)))
  (add-hook 'markdown-mode-hook 'flycheck-mode)
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill))

;; Org
(use-package org
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (add-to-list 'org-src-lang-modes '("haskell". haskell))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-bullets-mode)))

;; Shell
(add-to-list 'auto-mode-alist '("bash_profile" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))

;; Etc
(use-package web-mode
  :ensure t
  :config
  )

;; Utility Functions
(defun me/kill-filepath ()
  "Copy the current buffer filename with path to clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer filepath '%s' to clipboard." filepath))))

;;
;; End Language configuration
;;

;; Load local-only settings file after reading the main init file, i.e. useful
;; when you need to override variables, etc.
;;
;; Don't throw a warning if it doesn't exist on disk.
(load "~/dotfiles/emacs.d/local-postload" 1)
