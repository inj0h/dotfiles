;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil
      site-run-file nil
      gc-cons-threshold 50000000 ; NOTE: 50 MB. If too big, we lose the speedup.
      gc-cons-percentage 0.9)

;; NOTE: Restore default garbage collection settings.
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 800000
                                      gc-cons-percentage 0.1)))

(setq initial-scratch-message
      ";; God's in his heaven. All's right with the world. ")

(setq uvar:default-column 80
      uvar:default-indent 4)

(defun ufun:add-local-vi-bindings (bind-modes)
  "Add vi-like local keybindings to BIND-MODES where BIND-MODES is a list of
mode hooks."
  (dolist (mode bind-modes)
    (add-hook mode
              '(lambda ()
                 (progn
                   (local-set-key (kbd "j") 'next-line)
                   (local-set-key (kbd "k") 'previous-line))))))

(defun ufun:add-word-to-dictionary ()
  "Add the word-at-point to aspell's dictionary. You can call this function
interactively."
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
  "Create KEYBINDINGS associated with a LEADER key based on a new KEYMAP for an
extant HOOK. Note for KEYMAP, the caller provides a new name with which this
function will create a new keymap. E.g. 'new-keymap-name.

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
  "Create KEYBINDINGS associated with a LEADER key based on a new KEYMAP for an
extant MODE map under a VIMODE context. Note for KEYMAP, the caller provides a
new name with which this function will create a new keymap. E.g.
'new-keymap-name.

This function will only work for Evil keybindings and exacts a vi motion state
i.e. VIMODE for which these keybindings apply.

This function exists to provide a (hopefully) lightweight solution to third
party packages like Evil-Leader and General."
  (progn
    (define-prefix-command keymap)
    (evil-define-key* vimode mode (kbd leader) keymap) ; NOTE: Don't use the macro!
    (ufun:create-keybindings keymap keybindings)))

(defun ufun:get-buffers-matching-mode (mode)
  "Return a list of buffers where their major-mode is equal to MODE.

Stolen from Mickey Petersen (Mastering Emacs author).
See https://masteringemacs.org/article/searching-buffers-occur-mode."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defun ufun:goto-previous-buffer ()
  "Return to the previously visited buffer. You can call this function
interactively."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ufun:kill-filepath ()
  "Copy the current buffer filename with path to clipboard. You can call this
function interactively."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer filepath '%s' to clipboard." filepath))))

(defun ufun:multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode. You can call
this function interactively.

Stolen from Mickey Petersen (Mastering Emacs author).
See https://masteringemacs.org/article/searching-buffers-occur-mode."
  (interactive)
  (multi-occur
   (ufun:get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun ufun:org-archive-confirm ()
  "Invoke `org-archive-subtree' with a single prefix argument, C-u in this case.
You can call this function interactively.

Programmatically, passing 4 as an argument to `org-archive-subtree' achieves the
same thing as calling C-u once. I.e. a single FIND-DONE for the
`org-archive-subtree' method."
  (interactive)
  (org-archive-subtree '(4)))

(defun ufun:project-recompile ()
  "Invoke `compilation-mode' on the current project with the previous settings
or return an appropriate error message in the minibuffer on failure.

You can call this function interactively."
  (interactive)
  (let ((comp-buffer "*compilation*"))
    (if (get-buffer comp-buffer)
        (progn
          (project-switch-to-buffer comp-buffer)
          (recompile)
          (evil-goto-line))
      (message "Error: You have not tried to compile this project yet."))))

(setq bookmark-set-fringe-mark nil
      flyspell-duplicate-distance 0 ; NOTE: Does not work on Mac.
      inhibit-startup-screen t)

(global-hl-line-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq dabbrev-case-distinction nil
      dabbrev-case-fold-search t
      dabbrev-case-replace nil)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(blink-cursor-mode 1)
(delete-selection-mode t)
(setq blink-cursor-blinks 30
      mouse-drag-copy-region nil
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 1))
      scroll-bar-adjust-thumb-portion nil ; NOTE: This only works on X11.
      scroll-preserve-screen-position t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq dired-listing-switches "-alo")

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq initial-frame-alist '((width . 90) (height . 35)))

;; NOTE: Render non-focused frames transparent. I.e. when setting the alpha (transparency level), the first and second numbers indicate focused and unfocused transparency respectively. 100 alpha means opaque.
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

(setq-default column-number-indicator-zero-based nil
              fill-column uvar:default-column)
(setq column-number-mode t
      display-line-numbers-grow-only t)

(add-hook 'minibuffer-setup-hook '(lambda () (setq truncate-lines nil)))

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)
(global-auto-revert-mode 1)

(setq ibuffer-default-sorting-mode 'filename/process)

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

(ufun:add-local-vi-bindings
 '(ibuffer-mode-hook
   org-agenda-mode-hook
   package-menu-mode-hook))

(setq org-directory "~/Documents"
      org-enforce-todo-dependencies t
      org-hide-emphasis-markers t
      org-indent-indentation-per-level 2
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-startup-folded t
      org-time-stamp-custom-formats '("<%Y.%m.%d %A>" . "<%Y.%m.%d %A %H:%M>")
      org-todo-keywords '((sequence "TODO(t)"
                                    "ACTIVE(a!)"
                                    "BLOCKED(b@/!)"
                                    "SHELVED(s!)"
                                    "|"
                                    "DONE(d!)"
                                    "CANCELED(c@/!)"))
      org-use-fast-todo-selection t)
(setq-default org-display-custom-times t)

(add-hook 'org-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))
(add-hook 'org-mode-hook 'org-indent-mode)

(with-eval-after-load 'org-agenda
  (progn
    (setq org-agenda-custom-commands
          `(("A" "Custom Agenda"
             ((todo "ACTIVE\|BLOCKED" ((org-agenda-overriding-header "You Can (Not) Do It\n\nCurrent:")))
              (agenda "" ((org-agenda-block-separator ?-)
                          (org-agenda-overriding-header "\nToday:")
                          (org-agenda-span 1)
                          (org-deadline-warning-days 0)
                          (org-scheduled-past-days 0)
                          (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                          (org-agenda-format-date "%Y.%m.%d %A")))
              (agenda "" ((org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\nNext Five Days:")
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-start-day "+1d") ; Start after 1 day to avoid overlap with the previous section.
                          (org-agenda-span 5)
                          (org-deadline-warning-days 0)
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-format-date "%Y.%m.%d %A")))
              (agenda "" ((org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\nNext Thirty Days:")
                          (org-agenda-time-grid nil)
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-start-day "+6d") ; Start after 5 days to avoid overlap with the previous section.
                          (org-agenda-span 30)
                          (org-agenda-show-all-dates nil)
                          (org-deadline-warning-days 0)
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-format-date "%Y.%m.%d %A")))))))
    (setq org-agenda-files (list org-directory))))

(setq org-capture-templates
      '(("m" "File Meeting" entry (file+headline "meetings.org" "Meetings") "* %?\n** Topics [/]\n** Notes ")
        ("n" "File Note" entry (file+headline "notes.org" "Jotted") "* %?")
        ("t" "File Task" entry (file+headline "todos.org" "Tasks") "* TODO %?\n** Subtasks [/]\n** Notes ")))

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(add-hook 'java-mode-hook '(lambda () (setq-local fill-column 120)))

(add-hook 'js-mode-hook 'prettify-symbols-mode)
(add-hook 'js-mode-hook '(lambda () (push '("=>" . "\u21d2") prettify-symbols-alist)))

(add-hook 'latex-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))
(add-hook 'latex-mode-hook 'flyspell-mode)

(add-hook 'nxml-mode-hook
          '(lambda ()
             (setq nxml-attribute-indent uvar:default-indent
                   nxml-child-indent uvar:default-indent)))

(setq sh-indentation uvar:default-indent)

(add-hook 'text-mode-hook '(lambda () (setq-local fill-column 72))) ; NOTE: Blame Git!
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

(set-frame-font "Iosevka-14" nil t) ; NOTE: Make sure the OS has this installed!

(setq require-final-newline t
      show-paren-delay 0
      sentence-end-double-space nil)

(show-paren-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

(setq-default indent-tabs-mode nil
              tab-width uvar:default-indent)
(setq c-basic-offset uvar:default-indent)

(cond ((equal system-type 'gnu/linux)
       (setq ispell-program-name "/usr/bin/aspell"))
      ((equal system-type 'darwin)
       (setq ispell-program-name "/usr/local/bin/aspell")))

(setq flyspell-default-dictionary "en_US")

(setq-default whitespace-line-column nil) ; NOTE: Use fill-column setting.
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq tramp-default-method "ssh")

(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell 1)

;; (setq url-proxy-services
;;       '(("http"  . "proxy:port")
;;         ("https" . "proxy:port")))

;; TODO: Refactor this code so that it correctly installs missing packages.
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (packages '(diminish
                    evil
                    evil-escape
                    json-mode
                    kuronami-theme
                    markdown-mode
                    nix-mode
                    org-bullets
                    rust-mode
                    swift-mode
                    toml-mode
                    typescript-mode
                    undo-fu
                    yaml-mode
                    zig-mode))
  (when (not (package-installed-p packages))
    (package-install packages)))

(load-theme 'kuronami t)

(add-hook 'org-mode-hook 'org-bullets-mode)

(require 'evil)
(require 'evil-escape)
(evil-mode 1)
(evil-escape-mode t)
(evil-select-search-module 'evil-search-module 'evil-search)

(define-key evil-insert-state-map "\C-n" 'hippie-expand)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)

(with-eval-after-load 'org
  (evil-define-key 'motion org-mode-map (kbd "<tab>") 'org-cycle))

(setq-default evil-escape-key-sequence "hh"
              evil-escape-excluded-states '(normal visual motion)
              evil-escape-delay 0.2)

(ufun:create-keybindings
 evil-motion-state-map
 '((";"  . evil-ex)
   (":"  . evil-repeat-find-char)
   ("gc" . comment-dwim)
   ("zg" . ufun:add-word-to-dictionary)))

(define-prefix-command 'uvar:evil-leader-keymap)

;; NOTE: Using evil-define-key here will not bind additional mappings from other plugins for some reason. We need to use define-key.
(define-key evil-motion-state-map (kbd "SPC") 'uvar:evil-leader-keymap)

(setq uvar:evil-leader-bindings
      '((",," . org-capture)
        (",m" . (lambda () (interactive) (org-capture nil "m")))
        (",n" . (lambda () (interactive) (org-capture nil "n")))
        (",t" . (lambda () (interactive) (org-capture nil "t")))
        ("."  . ibuffer)
        ("pd" . project-forget-project)
        ("pk" . project-kill-buffers)
        ("ps" . project-switch-project)
        ("C"  . project-compile)
        ("c"  . ufun:project-recompile)
        ("r"  . ufun:goto-previous-buffer)
        ("la" . align-regexp)
        ("lc" . count-words-region)
        ("lo" . occur)
        ("lP" . multi-occur-in-matching-buffers)
        ("lp" . ufun:multi-occur-in-this-mode)
        ("ls" . sort-lines)
        ("A"  . (lambda () (interactive) (org-agenda nil "A")))
        ("a"  . apropos)
        ("O"  . switch-to-buffer)
        ("o"  . project-switch-to-buffer)
        ("E"  . find-file)
        ("e"  . project-find-file)
        ("T"  . eval-expression)
        ("t"  . execute-extended-command)
        ("n"  . yank-pop)
        ("s"  . server-edit)
        ("bl" . bookmark-bmenu-list)
        ("bs" . bookmark-set)
        ("W"  . whitespace-cleanup)
        ("w"  . whitespace-mode)))

(ufun:create-keybindings uvar:evil-leader-keymap uvar:evil-leader-bindings)

;; NOTE: The following keybindings only affect the particular mode.

(ufun:create-leader-local-keybindings
 "SPC"
 'compilation-mode-hook
 'uvar:evil-leader-compilation-keymap
 (append uvar:evil-leader-bindings
         '(("mr" . (lambda ()
                     (interactive) (progn (recompile) (evil-goto-line)))))))

(ufun:create-leader-local-keybindings
 "SPC"
 'dired-mode-hook
 'uvar:evil-leader-dired-keymap
 (append uvar:evil-leader-bindings
         '(("mG" . end-of-buffer)
           ("mg" . beginning-of-buffer)
           ("mw" . wdired-change-to-wdired-mode))))

(add-hook 'ibuffer-mode-hook
          '(lambda () (local-set-key (kbd "SPC") 'uvar:evil-leader-keymap)))

(ufun:create-leader-evil-keybindings
 "SPC"
 emacs-lisp-mode-map
 'motion
 'uvar:evil-leader-elisp-keymap
 (append uvar:evil-leader-bindings '(("me" . eval-last-sexp))))

(with-eval-after-load 'org
  (ufun:create-leader-evil-keybindings
   "SPC"
   org-mode-map
   'motion
   'uvar:evil-leader-org-keymap
   (append uvar:evil-leader-bindings
           '(("mA" . ufun:org-archive-confirm)
             ("ma" . org-archive-subtree)
             ("mc" . org-copy-subtree)
             ("mD" . (lambda () (interactive) (org-deadline '(4))))
             ("md" . org-deadline)
             ("mi" . org-insert-heading)
             ("mS" . (lambda () (interactive) (org-schedule '(4))))
             ("ms" . org-schedule)
             ("mx" . org-cut-subtree)))))

(require 'diminish)
(diminish 'evil-escape-mode)
(with-eval-after-load 'org-indent (diminish 'org-indent-mode))
(with-eval-after-load 'subword (diminish 'subword-mode))

(with-eval-after-load 'json-mode
  (progn
    (setq js-indent-level uvar:default-indent)
    (add-to-list 'auto-mode-alist '("\\.eslintrc\\'"   . json-mode))
    (add-to-list 'auto-mode-alist '("\\.prettierrc\\'" . json-mode))))

(with-eval-after-load 'markdown-mode
  (progn
    (cond ((string-equal system-type "gnu/linux")
           (setq markdown-command "/usr/bin/pandoc"))
          ((string-equal system-type "darwin")
           (setq markdown-command "/usr/local/bin/pandoc")))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
    (add-hook 'markdown-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook '(lambda () (setq-local fill-column uvar:default-column)))))

(with-eval-after-load 'swift-mode
  (setq swift-mode:basic-offset uvar:default-indent))

(with-eval-after-load 'typescript-mode
  (progn
    (setq typescript-indent-level uvar:default-indent)
    (add-hook 'typescript-mode-hook 'prettify-symbols-mode)
    (add-hook 'typescript-mode-hook '(lambda () (push '("=>" . "\u21d2") prettify-symbols-alist)))))

(with-eval-after-load 'yaml-mode
  (setq yaml-indent-offset uvar:default-indent))

(with-eval-after-load 'zig-mode
  (progn
    (setq zig-format-on-save nil)
    (add-hook 'zig-mode-hook '(lambda () (setq-local fill-column 100)))))