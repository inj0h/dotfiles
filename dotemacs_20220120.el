;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil
      site-run-file nil
      gc-cons-threshold 50000000 ; 50 MB. If too big, we lose the speedup.
      gc-cons-percentage 0.9)

;; Restore default garbage collection settings.
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 800000
                                      gc-cons-percentage 0.1)))

(setq initial-scratch-message
      ";; God's in his heaven. All's right with the world. ")

(setq uvar:default-column 80
      uvar:default-indent 4)

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

(defun ufun:create-keybindings (keymap keybindings)
  "Create KEYBINDINGS based on an existing KEYMAP."
  (dolist (binding keybindings)
    (define-key keymap
      (kbd (car binding)) (cdr binding))))

(defun ufun:create-leader-local-keybindings (leader hook keymap keybindings)
  "Create KEYBINDINGS associated with a LEADER key based on an extant KEYMAP for
an extant HOOK.

This function exists to provide a (hopefully) lightweight solution to third
party packages like Evil-Leader and General.

Online resources used to learn about backticks in Emacs Lisp.
- https://stackoverflow.com/questions/30150186/what-does-backtick-mean-in-lisp
- https://stackoverflow.com/questions/26613583/emacs-use-add-hook-inside-function-defun"
  (progn
    (define-prefix-command keymap)
    (add-hook hook `(lambda () (local-set-key (kbd ,leader) ,keymap)))
    (ufun:create-keybindings keymap keybindings)))

(defun ufun:create-leader-evil-keybindings (leader mode vimode keymap keybindings)
  "Create KEYBINDINGS associated with a LEADER key based on an extant KEYMAP for
an extant MODE map under a VIMODE context.

This function will only work for Evil keybindings and exacts a vi motion state
i.e. VIMODE for which these keybindings apply.

This function exists to provide a (hopefully) lightweight solution to third
party packages like Evil-Leader and General."
  (progn
    (define-prefix-command keymap)
    (evil-define-key* vimode mode (kbd leader) keymap) ; Tip - Don't use the macro!
    (ufun:create-keybindings keymap keybindings)))

(defun ufun:goto-previous-buffer ()
  "Return to the previously visited buffer. This function is interactive."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ufun:kill-filepath ()
  "Copy the current buffer filename with path to clipboard. This function is
interactive."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer filepath '%s' to clipboard." filepath))))

(defun ufun:org-archive-confirm ()
  "Provide an interactive call to `org-archive-subtree' with a single prefix
argument, C-u in this case.

Programmatically, passing 4 as an argument to `org-archive-subtree' achieves the
same thing as calling C-u once. I.e. a single FIND-DONE for the
`org-archive-subtree' method."
  (interactive)
  (org-archive-subtree '(4)))

(setq flyspell-duplicate-distance 0 ; Does not work on Emacs 27.2 on Mac.
      inhibit-startup-screen      t
      vc-handled-backends         nil)

(global-hl-line-mode -1)
(menu-bar-mode       -1)
(scroll-bar-mode     -1)
(tool-bar-mode       -1)

;; Highlight the current line.
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook          'hl-line-mode)
(add-hook 'ibuffer-mode-hook        'hl-line-mode)
(add-hook 'org-mode-hook            'hl-line-mode)
(add-hook 'package-menu-mode-hook   'hl-line-mode)

(setq mouse-drag-copy-region nil
      blink-cursor-blinks 30)

(blink-cursor-mode 1)
(delete-selection-mode t)

(add-hook 'server-visit-hook '(lambda () (xterm-mouse-mode 1))) ; Terminal mousing.

(setq scroll-bar-adjust-thumb-portion nil) ; No over-scrolling (X11 only).

;; Smooth scrolling kinda...
;; NOTE: scroll-preserve-screen-position gets really weird with Evil.
(setq mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 1)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq dired-listing-switches "-alo")

;; Dimensions of the frame on load.
(setq initial-frame-alist '((width . 90) (height . 35)))

;; Render non-focused frames transparent.
;;
;; I.e. when setting the alpha (transparency level), the first and second
;; numbers indicate focused and unfocused transparency respectively. 100 alpha
;; means opaque.
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

(setq-default column-number-indicator-zero-based nil ; Count columns starting from 1, /i.e./ the default is 0.
              fill-column uvar:default-column)
(setq column-number-mode t)

;; Keep uniform width. I.e. if the file has 100 lines then single and double
;; digit numbers take up 3 spaces.
(setq display-line-numbers-grow-only t)

(add-hook 'minibuffer-setup-hook '(lambda () (setq truncate-lines nil))) ; No minibuffer line wrapping.

(setq auto-save-default nil
      create-lockfiles  nil
      make-backup-files nil)
(global-auto-revert-mode 1) ; Auto-reload files on change.

(setq ibuffer-default-sorting-mode 'filename/process
      ibuffer-default-sorting-reversep t)

(setq ido-auto-merge-work-directories-length -1
      ido-case-fold                           t
      ido-enable-flex-matching                t
      ido-everywhere                          t)
(ido-mode 1)

(setq uvar:isearch-mode-keybindings
      '(("<up>"   . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)))

(add-hook 'isearch-mode-hook
          '(lambda ()
             (dolist (bindings uvar:isearch-mode-keybindings)
               (define-key isearch-mode-map
                 (kbd (car bindings)) (cdr bindings)))))

(add-hook 'ibuffer-mode-hook      '(lambda () (local-set-key (kbd "j") 'next-line)))
(add-hook 'ibuffer-mode-hook      '(lambda () (local-set-key (kbd "k") 'previous-line)))
(add-hook 'package-menu-mode-hook '(lambda () (local-set-key (kbd "j") 'next-line)))
(add-hook 'package-menu-mode-hook '(lambda () (local-set-key (kbd "k") 'previous-line)))

(setq org-enforce-todo-dependencies t
      org-hide-emphasis-markers     t
      org-src-fontify-natively      t
      org-src-tab-acts-natively     t
      org-startup-folded            t
      org-time-stamp-formats        '("<%Y_%m_%d %a>" . "<%Y_%m_%d %a %H:%M>")
      org-todo-keywords             '((sequence "TODO(t)"
                                                "IN-PROGRESS(p!)"
                                                "BLOCKED(b@/!)"
                                                "SOMEDAY(s@/!)"
                                                "|"
                                                "DONE(d!)"
                                                "CANCELED(c@/!)"))
      org-use-fast-todo-selection   t)
(add-hook 'org-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(add-hook 'java-mode-hook '(lambda () (setq-local fill-column 120)))

(add-hook 'latex-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))
(add-hook 'latex-mode-hook 'flyspell-mode)

(add-hook 'nxml-mode-hook '(lambda () (setq nxml-attribute-indent uvar:default-indent)))
(add-hook 'nxml-mode-hook '(lambda () (setq nxml-child-indent     uvar:default-indent)))

(setq sh-indentation uvar:default-indent)

(add-hook 'text-mode-hook '(lambda () (setq-local fill-column 72))) ; Blame Git.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . text-mode))

(require 'server)
(unless (server-running-p) (server-start))

(add-hook 'tetris-mode-hook
          '(lambda ()
             (ufun:create-keybindings
              tetris-mode-map
              '(("," . tetris-rotate-prev)
                ("a" . tetris-move-left)
                ("o" . tetris-move-down)
                ("e" . tetris-move-right)))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)

(set-frame-font "Iosevka-14" nil t) ; Make sure the OS has this installed!

(setq require-final-newline t
      show-paren-delay 0
      sentence-end-double-space nil)

(show-paren-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

(setq-default indent-tabs-mode nil           ; No tabs!
              tab-width uvar:default-indent) ; Use four spaces!
(setq c-basic-offset uvar:default-indent)

(cond ((equal system-type 'gnu/linux)
       (setq ispell-program-name "/usr/bin/aspell"))
      ((equal system-type 'darwin)
       (setq ispell-program-name "/usr/local/bin/aspell")))

(setq-default whitespace-line-column nil) ; Use fill-column setting.
(add-hook 'before-save-hook 'whitespace-cleanup)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell 1)

;; E.g.
;; (setq url-proxy-services
;;       '(("http"  . "work.proxy.com:8080")
;;         ("https" . "work.proxy.com:8080")))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (packages '(company
                    evil
                    evil-escape
                    json-mode
                    markdown-mode
                    naysayer-theme
                    org-bullets
                    rust-mode
                    swift-mode
                    toml-mode
                    typescript-mode
                    undo-fu
                    yaml-mode))
  (when (not (package-installed-p packages))
    (package-install packages)))

;; Theme
(load-theme 'naysayer t) ; This is (not) a compiler stream.

;; Org
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(require 'company)
(setq company-idle-delay 0)
(setq-default company-dabbrev-downcase nil
              company-dabbrev-ignore-case 1)

(global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-t") #'company-select-previous))

(add-hook 'markdown-mode-hook '(lambda () (company-mode -1)))
(add-hook 'tex-mode-hook      '(lambda () (company-mode -1)))
(add-hook 'text-mode-hook     '(lambda () (company-mode -1)))

(require 'evil)
(require 'undo-fu)
(require 'evil-escape)
(evil-mode 1)
(evil-escape-mode t)
(evil-select-search-module 'evil-search-module 'evil-search)

(define-key evil-normal-state-map "u"    'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

(setq-default evil-escape-key-sequence    "hh"
              evil-escape-excluded-states '(normal visual motion)
              evil-escape-delay           0.2)

(ufun:create-keybindings
 evil-motion-state-map
 '((";"  . evil-ex)
   (":"  . evil-repeat-find-char)
   ("gc" . comment-dwim)
   ("zg" . ufun:add-word-to-dictionary)))

(define-key evil-emacs-state-map  (kbd "C-M-s-m") 'evil-exit-emacs-state)
(define-key evil-motion-state-map (kbd "C-M-s-m") 'evil-emacs-state)

;; Have Ctrl-z suspend the frame, i.e. reclaim STDIO with Emacsclient.
(define-key evil-emacs-state-map  (kbd "C-z") 'suspend-frame)
(define-key evil-motion-state-map (kbd "C-z") 'suspend-frame)

(define-prefix-command 'uvar:evil-leader-keymap)

;; Using evil-define-key here will not bind additional mappings from other
;; plugins via use-package :bind for whatever reason. Need to use define-key.
(define-key evil-motion-state-map (kbd "SPC") 'uvar:evil-leader-keymap)

(setq uvar:evil-leader-bindings
      '((",," . bookmark-bmenu-list)
        (",s" . bookmark-set)
        ("."  . ibuffer)
        ("c"  . compile)
        ("r"  . ufun:goto-previous-buffer)
        ("la" . align-regexp)
        ("lc" . count-words-region)
        ("ls" . sort-lines)
        ("a"  . apropos)
        ("O"  . occur)
        ("o"  . switch-to-buffer)
        ("e"  . find-file)
        ("T"  . eval-expression)
        ("t"  . execute-extended-command)
        ("n"  . yank-pop)
        ("W"  . whitespace-cleanup)
        ("w"  . whitespace-mode)))

(ufun:create-keybindings uvar:evil-leader-keymap uvar:evil-leader-bindings)

;; The following keybindings only affect the particular mode.

;; Dired
(ufun:create-leader-local-keybindings
 "SPC"
 'dired-mode-hook
 'uvar:evil-leader-dired-keymap
 (append uvar:evil-leader-bindings
         '(("mG" . end-of-buffer)
           ("mg" . beginning-of-buffer)
           ("mw" . wdired-change-to-wdired-mode))))

;; Ibuffer
(add-hook 'ibuffer-mode-hook
          '(lambda () (local-set-key (kbd "SPC") 'uvar:evil-leader-keymap)))

;; Elisp
(ufun:create-leader-evil-keybindings
 "SPC"
 emacs-lisp-mode-map
 'motion
 'uvar:evil-leader-elisp-keymap
 (append uvar:evil-leader-bindings '(("me" . eval-last-sexp))))

;; Org
(ufun:create-leader-evil-keybindings
 "SPC"
 org-mode-map
 'motion
 'uvar:evil-leader-org-keymap
 (append uvar:evil-leader-bindings
         '(("mA" . ufun:org-archive-confirm)
           ("ma" . org-archive-subtree)
           ("mc" . org-copy-subtree)
           ("md" . org-demote-subtree)
           ("mi" . org-insert-heading)
           ("mp" . org-promote-subtree)
           ("mx" . org-cut-subtree))))

(require 'json-mode)
(setq js-indent-level uvar:default-indent)
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'"   . json-mode))
(add-to-list 'auto-mode-alist '("\\.prettierrc\\'" . json-mode))

(require 'markdown-mode)
(cond ((string-equal system-type "gnu/linux")
       (setq markdown-command "/usr/bin/pandoc"))
      ((string-equal system-type "darwin")
       (setq markdown-command "/usr/local/bin/pandoc")))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)) ; Use GitHub flavored Markdown.
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))

(require 'rust-mode)

(require 'swift-mode)
(setq swift-mode:basic-offset uvar:default-indent)

(require 'toml-mode)

(require 'typescript-mode)
(setq typescript-indent-level uvar:default-indent)
(add-hook 'typescript-mode-hook 'prettify-symbols-mode)
(add-hook 'typescript-mode-hook '(lambda () (push '("=>" . "\u21d2") prettify-symbols-alist)))

(require 'yaml-mode)
(setq yaml-indent-offset uvar:default-indent)
