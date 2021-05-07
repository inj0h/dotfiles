;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil
      site-run-file nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Revert the garbage collection to the default value, i.e. threshold = 800000
;; and percentage at 0.1.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000
                                          gc-cons-percentage 0.1)))

(setq uvar:default-column 80
      uvar:default-indent 2)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-hook 'dired-mode-hook 'hl-line-mode)

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(when (equal system-type 'darwin)
  (setq default-directory "~/"))

(global-auto-revert-mode 1)

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "G") 'end-of-buffer)))
(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "R") 'ibuffer-do-replace-regexp)))
(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "g") 'beginning-of-buffer)))
(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "j") 'next-line)))
(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "k") 'previous-line)))
(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "r") 'ibuffer-update)))

(add-hook 'ibuffer-mode-hook 'hl-line-mode)

(setq ido-auto-merge-work-directories-length -1
      ido-case-fold t
      ido-enable-flex-matching t
      ido-everywhere t)

(ido-mode 1)

(setq uvar:isearch-mode-keybindings
      '(("<up>"   . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)))

(add-hook 'isearch-mode-hook
          '(lambda ()
             (dolist (bindings uvar:isearch-mode-keybindings)
               (define-key isearch-mode-map
                 (kbd (car bindings)) (cdr bindings)))))

(setq uvar:emacs-custom-keybindings
      '(("<M-down>"  . windmove-down)
        ("<M-left>"  . windmove-left)
        ("<M-right>" . windmove-right)
        ("<M-up>"    . windmove-up)))

(dolist (keybindings uvar:emacs-custom-keybindings)
  (global-set-key (kbd (car keybindings)) (cdr keybindings)))

(add-hook 'c-mode-hook   'flyspell-prog-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(add-hook 'java-mode-hook '(lambda () (setq-local fill-column 120)))
(add-hook 'java-mode-hook '(lambda () (setq-local whitespace-line-column 120)))
(add-hook 'java-mode-hook 'flyspell-prog-mode)

(add-hook 'latex-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))
(add-hook 'latex-mode-hook 'flyspell-mode)

(setq sh-indentation uvar:default-indent)
(add-hook 'sh-mode-hook 'flyspell-prog-mode)

(add-hook 'text-mode-hook '(lambda () (setq-local fill-column 72)))            ; blame Git
(add-hook 'text-mode-hook '(lambda () (setq-local whitespace-line-column 72))) ; same
(add-hook 'text-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . text-mode))

(delete-selection-mode t)

(setq mouse-drag-copy-region 1)

(setq org-enforce-todo-dependencies t
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-time-stamp-formats '("<%Y_%m_%d %a>" .
                               "<%Y_%m_%d %a %H:%M>")
      org-todo-keywords '((sequence "TODO(t)"
                                    "IN-PROGRESS(p!)"
                                    "BLOCKED(b@/!)"
                                    "SOMEDAY(s@/!)"
                                    "|"
                                    "DONE(d!)"
                                    "CANCELED(c@/!)"))
      org-use-fast-todo-selection t)
(add-hook 'org-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))
(add-hook 'org-mode-hook '(lambda () (setq-local whitespace-line-column uvar:default-column)))

(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook '(lambda () (local-set-key (kbd "G")  'end-of-buffer)))
(add-hook 'package-menu-mode-hook '(lambda () (local-set-key (kbd "gg") 'beginning-of-buffer)))
(add-hook 'package-menu-mode-hook '(lambda () (local-set-key (kbd "j")  'next-line)))
(add-hook 'package-menu-mode-hook '(lambda () (local-set-key (kbd "k")  'previous-line)))

(when (equal system-type 'darwin)
  (let ((mac-binaries '("/usr/local/bin")))
    (setenv "PATH" (mapconcat 'identity mac-binaries path-separator))
    (dolist (binaries mac-binaries) (add-to-list 'exec-path binaries))))

(require 'server)
(unless (server-running-p) (server-start))

(cond ((equal system-type 'gnu/linux)
       (setq ispell-program-name "/usr/bin/aspell"))
      ((equal system-type 'darwin)
       (setq ispell-progam-name "/usr/local/bin/aspell")))

(setq backward-delete-char-untabify-method 'hungry)

(setq require-final-newline t)

(setq show-paren-delay 0)
(show-paren-mode 1)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width uvar:default-indent)
(setq c-basic-offset uvar:default-indent)

(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'prog-mode-hook 'subword-mode)

(setq-default column-number-indicator-zero-based nil)
(setq column-number-mode t)

(setq-default fill-column uvar:default-column)
(setq-default whitespace-line-column fill-column)

(blink-cursor-mode 1)
(setq blink-cursor-blinks 30)

(add-hook 'server-visit-hook '(lambda () (xterm-mouse-mode 1)))

(global-hl-line-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq frame-title-format nil inhibit-startup-screen t)

(set-frame-font "Inconsolata-15" nil t)

(setq initial-frame-alist '((width . 100) (height . 48)))

(set-frame-parameter (selected-frame) 'alpha '(100 . 97))
(add-to-list 'default-frame-alist '(alpha . (100 . 97)))

(setq display-line-numbers-grow-only t)

(add-hook 'minibuffer-setup-hook '(lambda () (setq truncate-lines nil)))

(setq scroll-bar-adjust-thumb-portion nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-preserve-screen-position t
      scroll-step 1)

(setq initial-scratch-message
      ";; God's in his heaven. All's right with the world. ")

(setq visible-bell 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun ufun:add-word-to-dictionary ()
  "Add the word-at-point to aspell's dictionary."
  (interactive)
  (let ((current-location (point)) (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save
                           nil
                           (car word)
                           current-location
                           (cadr word)
                           (caddr word)
                           current-location))))

(defun ufun:goto-previous-buffer ()
  "Return to the previously visited buffer. This function is
     interactive."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ufun:kill-filepath ()
  "Copy the current buffer filename with path to clipboard. This
     function is interactive."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer filepath '%s' to clipboard." filepath))))

(setq vc-handled-backends nil)

;; E.g.
;; (setq url-proxy-services
;;       '(("http"  . "work.proxy.com:8080")
;;         ("https" . "work.proxy.com:8080")))

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (setq uvar:diminished-modes
        '(auto-fill-function ; = auto-fill-mode
          eldoc-mode))

  (dolist (diminished uvar:diminished-modes) (diminish diminished))

  (setq uvar:forced-diminished-modes '((subword . subword-mode)))

  (dolist (diminished uvar:forced-diminished-modes)
    (with-eval-after-load (car diminished) (diminish (cdr diminished)))))

(use-package naysayer-theme
  :ensure t
  :demand
  :config (load-theme 'naysayer t))

(use-package org-bullets
  :ensure t
  :defer t
  :hook ((org-mode . hl-line-mode)
         (org-mode . org-bullets-mode)))

(use-package evil
  :ensure t
  :demand
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (use-package undo-fu ; No BS. Linear undo.
    :ensure t
    :config
    (define-key evil-normal-state-map "u"    'undo-fu-only-undo)
    (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)))

(setq uvar:evil-motion-state-rebindings
      '((";"  . evil-ex)
        (":"  . evil-repeat-find-char)
        ("gc" . comment-dwim)
        ("zg" . ufun:add-word-to-dictionary)))

;; TODO: Make this into a general function or macro.
(dolist (keybindings uvar:evil-motion-state-rebindings)
  (define-key evil-motion-state-map
    (kbd (car keybindings)) (cdr keybindings)))

(define-key evil-emacs-state-map  (kbd "C-M-s-m") 'evil-exit-emacs-state)
(define-key evil-motion-state-map (kbd "C-M-s-m") 'evil-emacs-state)

;; Have Ctrl-z suspend the frame, i.e. reclaim STDIO with Emacsclient.
(define-key evil-emacs-state-map  (kbd "C-z") 'suspend-frame)
(define-key evil-motion-state-map (kbd "C-z") 'suspend-frame)

(define-prefix-command 'uvar:evil-leader-keymap)

;; Using evil-define-key here will not bind additional mappings from other
;; plugins via use-package :bind for whatever reason. Need to use define-key.
(define-key evil-motion-state-map
  (kbd "<SPC>") 'uvar:evil-leader-keymap)

(setq uvar:evil-leader-bindings
      '((",," . bookmark-bmenu-list)
        (",d" . bookmark-delete)
        (",s" . bookmark-set)
        ("."  . ibuffer)
        ("c"  . compile)
        ("r"  . ufun:goto-previous-buffer)
        ("la" . align-regexp)
        ("lc" . count-words-region)
        ("le" . ufun:evil-apply-macro-to-region-lines)
        ("lo" . occur)
        ("ls" . sort-lines)
        ("lw" . whitespace-mode)
        ("O"  . switch-to-buffer-other-window)
        ("o"  . switch-to-buffer)
        ("E"  . find-file-other-window)
        ("e"  . find-file)
        ("t"  . execute-extended-command)
        ("n"  . yank-pop)
        ("sp" . ufun:kill-filepath)))

(dolist (keybindings uvar:evil-leader-bindings)
  (define-key uvar:evil-leader-keymap
    (kbd (car keybindings)) (cdr keybindings)))

(define-prefix-command 'uvar:evil-leader-dired-keymap)

(add-hook 'dired-mode-hook
          '(lambda ()
             (local-set-key (kbd "SPC") 'uvar:evil-leader-dired-keymap)))

(setq uvar:evil-leader-bindings-dired
      (append uvar:evil-leader-bindings
              '(("mG" . end-of-buffer)
                ("mg" . beginning-of-buffer)
                ("mw" . wdired-change-to-wdired-mode))))

(dolist (keybindings uvar:evil-leader-bindings-dired)
  (define-key uvar:evil-leader-dired-keymap
    (kbd (car keybindings)) (cdr keybindings)))

(add-hook 'ibuffer-mode-hook '(lambda () (local-set-key (kbd "SPC") 'uvar:evil-leader-keymap)))

(define-prefix-command 'uvar:evil-leader-elisp-keymap)

(evil-define-key 'motion emacs-lisp-mode-map
  (kbd "<SPC>") 'uvar:evil-leader-elisp-keymap)

(setq uvar:evil-leader-bindings-elisp
      (append uvar:evil-leader-bindings
              '(("me" . eval-last-sexp))))

(dolist (keybindings uvar:evil-leader-bindings-elisp)
  (define-key uvar:evil-leader-elisp-keymap
    (kbd (car keybindings)) (cdr keybindings)))

(define-prefix-command 'uvar:evil-leader-org-keymap)

(evil-define-key 'motion org-mode-map
  (kbd "<SPC>") 'uvar:evil-leader-org-keymap)

(setq uvar:evil-leader-bindings-org
      (append uvar:evil-leader-bindings
              '(("mc" . org-copy-subtree)
                ("md" . org-demote-subtree)
                ("mi" . org-insert-heading)
                ("mp" . org-promote-subtree)
                ("mx" . org-cut-subtree))))

(dolist (keybindings uvar:evil-leader-bindings-org)
  (define-key uvar:evil-leader-org-keymap
    (kbd (car keybindings)) (cdr keybindings)))

(use-package evil-escape
  :ensure t
  :diminish
  :config
  (evil-escape-mode t)
  (setq-default evil-escape-key-sequence "hh"
                evil-escape-excluded-states '(normal visual motion)
                evil-escape-delay 0.2))

(use-package evil-numbers
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

(defun ufun:evil-apply-macro-to-region-lines ()
  "Easy binding for running an Evil macro over some selected lines."
  (interactive)
  (evil-ex "'<,'>norm@"))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :defer 5
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :defer 2
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1
        which-key-sort-order 'which-key-key-order-alpha)

  (which-key-add-keymap-based-replacements evil-motion-state-map
    "<SPC> ," "bookmark actions"
    "<SPC> ." "buffer menu"
    "<SPC> l" "line actions"
    "<SPC> r" "visit last buffer"
    "<SPC> O" "open buffer in other window"
    "<SPC> o" "open buffer"
    "<SPC> E" "edit file in other window"
    "<SPC> e" "edit file"
    "<SPC> t" "toplevel"
    "<SPC> s" "special actions")

  (which-key-add-major-mode-key-based-replacements 'dired-mode
    "<SPC> m" "dired actions")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "<SPC> m" "elisp actions")

  (which-key-add-major-mode-key-based-replacements 'org-mode
    "<SPC> m" "org actions")

  (which-key-mode))

(use-package fish-mode
  :ensure t
  :defer t
  :hook ((fish-mode . flyspell-prog-mode))
  :config (setq fish-indent-offset uvar:default-indent))

(use-package gitignore-mode
  :ensure t
  :defer t
  :hook ((gitignore-mode . flyspell-prog-mode)))

(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode . flyspell-prog-mode)))

(use-package json-mode
  :ensure t
  :defer t
  :hook ((json-mode . flyspell-prog-mode))
  :config
  (setq js-indent-level uvar:default-indent)
  (add-to-list 'auto-mode-alist '("\\.eslintrc\\'"   . json-mode))
  (add-to-list 'auto-mode-alist '("\\.prettierrc\\'" . json-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . flyspell-mode)
         (markdown-mode . (lambda () (setq-local fill-column uvar:default-column)))
         (markdown-mode . (lambda () (setq-local whitespace-line-column uvar:default-column))))
  :config
  (cond ((string-equal system-type "gnu/linux")
         (setq markdown-command "/usr/bin/pandoc"))
        ((string-equal system-type "darwin")
         (setq markdown-command "/usr/local/bin/pandoc"))))

(use-package swift-mode
  :ensure t
  :defer t
  :hook ((swift-mode . flyspell-prog-mode))
  :config
  (setq swift-mode:basic-offset uvar:default-indent))

(use-package typescript-mode
  :ensure t
  :defer t
  :hook ((typescript-mode . (lambda () (push '("=>" . 8658) prettify-symbols-alist)))
         (typescript-mode . flyspell-prog-mode)
         (typescript-mode . prettify-symbols-mode))
  :config (setq typescript-indent-level uvar:default-indent))

(use-package yaml-mode
  :ensure t
  :defer t
  :hook ((yaml-mode-hook . flyspell-prog-mode)))
