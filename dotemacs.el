;; -*- lexical-binding: t -*-
;;
;; 00. Startup:
;;

;; 発信準備!/발신 준비!
;; You stole these GC hacks from the following sites.
;; - https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org
;; - https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; - https://so.nwalsh.com/2020/02/29/dot-emacs
(setq package-enable-at-startup nil
      site-run-file nil
      gc-cons-threshold 50000000 ; 50 MB. If too big, we lose the speedup.
      gc-cons-percentage 0.9)

;; Restore default garbage collection settings.
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 800000
                                      gc-cons-percentage 0.1)))

;; Get in the text editor!
(setq initial-scratch-message
      ";; God's in his heaven. All's right with the world. ")

;;
;; 01. User Variables:
;;

(setq injh:default-column 80
      injh:default-indent 4)

;;
;; 02. User Functions:
;;

(defun injh:add-local-vi-bindings (bind-modes)
  "Add vi-like local keybindings to BIND-MODES where BIND-MODES is a list of
mode hooks."
  (dolist (mode bind-modes)
    (add-hook mode
              '(lambda ()
                 (progn
                   (local-set-key (kbd "j") 'next-line)
                   (local-set-key (kbd "k") 'previous-line))))))


(defun injh:add-word-to-dictionary ()
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


(defun injh:compile (dir)
  "Invoke `compilation-mode' after selecting a directory and compilation command.

You can call this function interactively."
  (interactive "DSelect directory:") ; Need this "D" in the string.
  (let ((default-directory dir))
    (progn
      (call-interactively 'compile)
      (switch-to-buffer "*compilation*")
      (delete-other-windows)
      (evil-goto-line))))


(defun injh:compile-again ()
  "Invoke `compilation-mode' with the previous settings or return an appropriate
error message in the minibuffer .

You can call this function interactively."
  (interactive)
  (let ((comp-buffer "*compilation*"))
    (if (get-buffer comp-buffer)
        (progn
          (switch-to-buffer comp-buffer)
          (delete-other-windows)
          (recompile)
          (evil-goto-line))
      (message "Error: You have not tried to compile anything yet."))))


(defun injh:create-keybindings (keymap keybindings)
  "Create KEYBINDINGS based on an existing KEYMAP."
  (dolist (binding keybindings)
    (define-key keymap
      (kbd (car binding)) (cdr binding))))


(defun injh:create-leader-local-keybindings (leader hook keymap keybindings)
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
    (injh:create-keybindings keymap keybindings)))


(defun injh:create-leader-evil-keybindings (leader mode vimode keymap keybindings)
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
    (evil-define-key* vimode mode (kbd leader) keymap) ; Don't use the macro!
    (injh:create-keybindings keymap keybindings)))


(defun injh:get-buffers-matching-mode (mode)
  "Return a list of buffers where their major-mode is equal to MODE.

Stolen from Mickey Petersen (Mastering Emacs author).
See https://masteringemacs.org/article/searching-buffers-occur-mode."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))


(defun injh:goto-previous-buffer ()
  "Return to the previously visited buffer. You can call this function
interactively."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; TODO: Finish implementing this method.
;; (defun injh:grep (dir)
;;   (interactive "DIn directory:")
;;   (let ((default-directory dir))
;;     (call-interactively 'execute-extended-command)))


(defun injh:kill-filepath ()
  "Copy the current buffer filename with path to clipboard. You can call this
function interactively."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer filepath '%s' to clipboard." filepath))))


(defun injh:multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode. You can call
this function interactively.

Stolen from Mickey Petersen (Mastering Emacs author).
See https://masteringemacs.org/article/searching-buffers-occur-mode."
  (interactive)
  (multi-occur
   (injh:get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))


(defun injh:org-archive-confirm ()
  "Invoke `org-archive-subtree' with a single prefix argument, C-u in this case.
You can call this function interactively.

Programmatically, passing 4 as an argument to `org-archive-subtree' achieves the
same thing as calling C-u once. I.e. a single FIND-DONE for the
`org-archive-subtree' method."
  (interactive)
  (org-archive-subtree '(4)))


;;
;; 03. Disable:
;;

(setq auto-save-default nil
      bookmark-set-fringe-mark nil
      create-lockfiles nil
      flyspell-duplicate-distance 0 ; Broken on Mac.
      inhibit-startup-screen t
      make-backup-files nil)
(global-hl-line-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;
;; 04. Vanilla Settings:
;;

;; Custom File
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)

;; Font
(set-frame-font "Iosevka-14" nil t) ; Make sure the OS has this installed!

;; Formatting
(setq c-basic-offset injh:default-indent
      require-final-newline t
      sentence-end-double-space nil)
(setq-default fill-column injh:default-column
              indent-tabs-mode nil
              tab-width injh:default-indent)

;; Keybindings
(injh:add-local-vi-bindings
 '(bookmark-bmenu-mode-hook
   ibuffer-mode-hook
   org-agenda-mode-hook
   package-menu-mode-hook))

;; Minibuffer
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'minibuffer-setup-hook '(lambda () (setq truncate-lines nil)))

;; Mouse
(setq mouse-drag-copy-region nil
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 1))
      scroll-bar-adjust-thumb-portion nil ; This only works on X11.
      scroll-preserve-screen-position t)

;; Windows/Frames
(setq initial-frame-alist '((width . 90) (height . 35)))

;; Render non-focused frames transparent. I.e. when setting the alpha
;; (transparency level), the first and second numbers indicate focused and
;; unfocused transparency respectively. 100 alpha means opaque.
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

;;
;; 05. Vanilla Packages:
;;

(setq blink-cursor-blinks 30)
(blink-cursor-mode 1)

(setq column-number-mode t)
(setq-default column-number-indicator-zero-based nil)

(setq dabbrev-case-distinction nil
      dabbrev-case-fold-search t
      dabbrev-case-replace nil)

(delete-selection-mode t)

(setq dired-listing-switches "-alo")

(setq display-line-numbers-grow-only t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Flyspell/Ispell
(setq flyspell-default-dictionary "en_US")
(cond ((equal system-type 'gnu/linux)
       (setq ispell-program-name "/usr/bin/aspell"))
      ((equal system-type 'darwin)
       (setq ispell-program-name "/usr/local/bin/aspell")))

(global-auto-revert-mode 1)

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

(setq ibuffer-default-sorting-mode 'filename/process)

(setq ido-auto-merge-work-directories-length -1
      ido-case-fold t
      ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

(setq injh:isearch-mode-keybindings
      '(("<up>"   . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)))
(add-hook 'isearch-mode-hook
          '(lambda ()
             (dolist (bindings injh:isearch-mode-keybindings)
               (define-key isearch-mode-map
                 (kbd (car bindings)) (cdr bindings)))))

(require 'server)
(unless (server-running-p) (server-start))

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Subword Mode
(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'tetris-mode-hook
          '(lambda ()
             (injh:create-keybindings
              tetris-mode-map
              '(("," . tetris-rotate-prev)
                ("a" . tetris-move-left)
                ("o" . tetris-move-down)
                ("e" . tetris-move-right)))))

(setq tramp-default-method "ssh")

(setq visible-bell 1)

;; Whitespace Mode
(setq-default whitespace-line-column nil) ; Use fill-column value.
(add-hook 'before-save-hook 'whitespace-cleanup)

;;
;; 06. Vanilla Programming Language Packages:
;;

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(add-hook 'java-mode-hook '(lambda ()
                             (let ((java-indent 2))
                               (setq-local c-basic-offset java-indent
                                           tab-width java-indent
                                           fill-column 100))))

(add-hook 'js-mode-hook 'prettify-symbols-mode)
(add-hook 'js-mode-hook '(lambda () (push '("=>" . "\u21d2") prettify-symbols-alist)))

(add-hook 'latex-mode-hook '(lambda () (setq-local fill-column injh:default-column)))
(add-hook 'latex-mode-hook 'flyspell-mode)

(add-hook 'nxml-mode-hook
          '(lambda ()
             (setq nxml-attribute-indent injh:default-indent
                   nxml-child-indent injh:default-indent)))

(setq sh-indentation injh:default-indent)

(add-hook 'text-mode-hook '(lambda () (setq-local fill-column 72))) ; Blame Git!
(add-hook 'text-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . text-mode))

;;
;; 07. Org Mode:
;;

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

(add-hook 'org-mode-hook '(lambda () (setq-local fill-column injh:default-column)))
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

;;
;; 08. Package Management:
;;

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

;;
;; 09. Evil Mode:
;;

;; Summon the Editor of the Beast - VI VI VI.
;;
;; Keybindings tuned for EN-Dvorak. Don’t change default vi/Vim too much!
;;
;; This configuration uses custom Elisp code to recreate Vim leader keybinding
;; features that third party packages like “Evil Leader” and “General” provide.
(require 'evil)
(require 'evil-escape)
(require 'undo-fu)
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

(injh:create-keybindings
 evil-motion-state-map
 '((";"  . evil-ex)
   (":"  . evil-repeat-find-char)
   ("gc" . comment-dwim)
   ("zg" . injh:add-word-to-dictionary)))

(define-prefix-command 'injh:evil-leader-keymap)

;; Using evil-define-key here will not bind additional mappings from other plugins for some reason. We need to use define-key.
(define-key evil-motion-state-map (kbd "SPC") 'injh:evil-leader-keymap)

(setq injh:evil-leader-bindings
      '((",," . org-capture)
        (",m" . (lambda () (interactive) (org-capture nil "m")))
        (",n" . (lambda () (interactive) (org-capture nil "n")))
        (",t" . (lambda () (interactive) (org-capture nil "t")))
        ("."  . ibuffer)
        ("C"  . injh:compile)
        ("c"  . injh:compile-again)
        ("r"  . injh:goto-previous-buffer)
        ("la" . align-regexp)
        ("lc" . count-words-region)
        ("lo" . occur)
        ("lp" . injh:multi-occur-in-this-mode)
        ("ls" . sort-lines)
        ("A"  . (lambda () (interactive) (org-agenda nil "A")))
        ("a"  . apropos)
        ("o"  . switch-to-buffer)
        ("e"  . find-file)
        ("T"  . eval-expression)
        ("t"  . execute-extended-command)
        ("n"  . yank-pop)
        ("s"  . server-edit)
        ("bl" . bookmark-bmenu-list)
        ("bs" . bookmark-set)
        ("W"  . whitespace-cleanup)
        ("w"  . whitespace-mode)))

(injh:create-keybindings injh:evil-leader-keymap injh:evil-leader-bindings)

;; The following keybindings only affect the particular mode.

(injh:create-leader-local-keybindings
 "SPC"
 'compilation-mode-hook
 'injh:evil-leader-compilation-keymap
 (append injh:evil-leader-bindings
         '(("mk" . kill-compilation)
           ("mr" . (lambda ()
                     (interactive) (progn (recompile) (evil-goto-line)))))))

(injh:create-leader-local-keybindings
 "SPC"
 'dired-mode-hook
 'injh:evil-leader-dired-keymap
 (append injh:evil-leader-bindings
         '(("mG" . end-of-buffer)
           ("mg" . beginning-of-buffer)
           ("mw" . wdired-change-to-wdired-mode))))

(add-hook 'ibuffer-mode-hook
          '(lambda () (local-set-key (kbd "SPC") 'injh:evil-leader-keymap)))

(injh:create-leader-evil-keybindings
 "SPC"
 emacs-lisp-mode-map
 'motion
 'injh:evil-leader-elisp-keymap
 (append injh:evil-leader-bindings '(("me" . eval-last-sexp))))

(with-eval-after-load 'org
  (injh:create-leader-evil-keybindings
   "SPC"
   org-mode-map
   'motion
   'injh:evil-leader-org-keymap
   (append injh:evil-leader-bindings
           '(("mA" . injh:org-archive-confirm)
             ("ma" . org-archive-subtree)
             ("mc" . org-copy-subtree)
             ("mD" . (lambda () (interactive) (org-deadline '(4))))
             ("md" . org-deadline)
             ("mi" . org-insert-heading)
             ("mS" . (lambda () (interactive) (org-schedule '(4))))
             ("ms" . org-schedule)
             ("mx" . org-cut-subtree)))))

;;
;; 10. Non-Vanilla Packages:
;;

(require 'diminish)
(diminish 'evil-escape-mode)
(with-eval-after-load 'org-indent (diminish 'org-indent-mode))
(with-eval-after-load 'subword (diminish 'subword-mode))

(load-theme 'kuronami t)

(add-hook 'org-mode-hook 'org-bullets-mode)

;;
;; 11. Non-Vanilla Programming Language Packages:
;;

(with-eval-after-load 'json-mode
  (progn
    (setq js-indent-level injh:default-indent)
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
    (add-hook 'markdown-mode-hook '(lambda () (setq-local fill-column injh:default-column)))))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook '(lambda () (setq-local fill-column 99))))

(with-eval-after-load 'swift-mode
  (setq swift-mode:basic-offset injh:default-indent))

(with-eval-after-load 'typescript-mode
  (progn
    (setq typescript-indent-level injh:default-indent)
    (add-hook 'typescript-mode-hook 'prettify-symbols-mode)
    (add-hook 'typescript-mode-hook '(lambda () (push '("=>" . "\u21d2") prettify-symbols-alist)))))

(with-eval-after-load 'yaml-mode
  (setq yaml-indent-offset injh:default-indent))

(with-eval-after-load 'zig-mode
  (progn
    (setq zig-format-on-save nil)
    (add-hook 'zig-mode-hook '(lambda () (setq-local fill-column 100)))))
