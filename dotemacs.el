;; -*- lexical-binding: t -*-
;;; 00. Startup:

;; 発信準備!/발신 준비!
;; You stole these GC hacks from the following sites
;; - https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org
;; - https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; - https://so.nwalsh.com/2020/02/29/dot-emacs
(setq package-enable-at-startup nil
      site-run-file nil
      gc-cons-threshold 50000000 ; 50 MB - If too big, we lose the speedup
      gc-cons-percentage 0.9)

;; Restore default garbage collection settings
(add-hook 'emacs-startup-hook '(lambda ()
                                 (setq gc-cons-threshold 800000
                                       gc-cons-percentage 0.1)))

;; Get in the text editor!
(setq initial-scratch-message
      ";; God's in his heaven. All's right with the world. ")

;;; 01. User Variables:

(setq inj0h:default-column 80
      inj0h:default-indent 4)

;;; 02. User Functions:

(defun inj0h:add-local-vi-bindings (bind-modes)
  "Add vi-like local keybindings to BIND-MODES where BIND-MODES is a list of
mode hooks."
  (dolist (mode bind-modes)
    (add-hook mode
              '(lambda ()
                 (progn
                   (local-set-key (kbd "j") 'next-line)
                   (local-set-key (kbd "k") 'previous-line))))))

(defun inj0h:add-word-to-dictionary ()
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

(defun inj0h:compile (dir)
  "Invoke `compilation-mode' after selecting a directory and compilation
command. You can call this function interactively."
  (interactive "DSelect directory: ") ; Need this "D" in the string
  (let ((default-directory dir))
    (progn
      (call-interactively 'compile)
      (switch-to-buffer "*compilation*")
      (delete-other-windows))))

(defun inj0h:compile-again ()
  "Invoke `compilation-mode' with the previous settings or return an appropriate
error message in the minibuffer. You can call this function interactively."
  (interactive)
  (let ((comp-buffer "*compilation*"))
    (if (get-buffer comp-buffer)
        (progn
          (switch-to-buffer comp-buffer)
          (delete-other-windows)
          (recompile))
      (message "Error: You have not tried to compile anything yet."))))

(defun inj0h:compile-with-color ()
  "Colorize from `compilation-filter-start' to `point'.

Not original, but stolen from someplace on the Internet."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(defun inj0h:create-keybindings (keymap keybindings)
  "Create KEYBINDINGS based on an existing KEYMAP."
  (dolist (binding keybindings)
    (define-key keymap
      (kbd (car binding)) (cdr binding))))

(defun inj0h:create-leader-local-keybindings (leader hook keymap keybindings)
  "Create KEYBINDINGS associated with a LEADER key based on a new KEYMAP for an
extant HOOK. Note for KEYMAP, the caller provides a new name with which this
function will create a new keymap.

This function exists to provide a (hopefully) lightweight solution to third
party packages like Evil-Leader and General.

Online resources used to learn about backticks in Emacs Lisp.
- https://stackoverflow.com/questions/30150186/what-does-backtick-mean-in-lisp
- https://stackoverflow.com/questions/26613583/emacs-use-add-hook-inside-function-defun"
  (progn
    (define-prefix-command keymap)
    (add-hook hook `(lambda () (local-set-key (kbd ,leader) ,keymap)))
    (inj0h:create-keybindings keymap keybindings)))

(defun inj0h:create-leader-evil-keybindings (leader mode vimode keymap
                                                    keybindings)
  "Create KEYBINDINGS associated with a LEADER key based on a new KEYMAP for an
extant MODE map under a VIMODE context. Note for KEYMAP, the caller provides a
new name with which this function will create a new keymap.

This function will only work for Evil keybindings and exacts a vi motion state
i.e. VIMODE for which these keybindings apply.

This function exists to provide a (hopefully) lightweight solution to third
party packages like Evil-Leader and General."
  (progn
    (define-prefix-command keymap)
    (evil-define-key* vimode mode (kbd leader) keymap) ; Don't use the macro!
    (inj0h:create-keybindings keymap keybindings)))

(defun inj0h:get-from-list-else (list match else)
  "Given that LIST is a list of cons cells - E.g. returned from
`inj0h:zip-pair', return the first of its elements such that the symbol name of
its element's car value equals MATCH. Otherwise, return ELSE."
  (let ((current (car list)))
    (cond ((not list)
           else)
          ((string= match (symbol-name (car current)))
           (cdr current))
          (t (inj0h:get-from-list-else (cdr list) match else)))))
;; Tests:
;; (setq test (zip-pair '(:foo 0 :bar "bar" :baz)))
;; (inj0h:get-from-list-else test ":foo" "oops")
;; (inj0h:get-from-list-else test ":bar" "oops")
;; (inj0h:get-from-list-else test ":baz" "oops")

(defun inj0h:grep-from-here (query)
  "Run system grep from the current buffer's directory against the QUERY regexp.
Correct behavior assumes an installation of grep. Please refer to the function
implementation for included grep arguments.

You can call this function interactively."
  (interactive "sgrep: ")
  (let ((grep-args (concat "grep"
                          " --color"
                          " --exclude-dir={.git,.idea,build,dist,node_modules,target}"
                          " --exclude 'Cargo.lock'"
                          " -Iinr "
                          "\"" query "\""
                          " .")))
    (grep grep-args)))

(defun inj0h:zip-pair (list)
  "Return a list of cons cells from LIST.

If a LIST of size n has an odd number of elements, then return a list of cons
cells for elements n-1.

For a LIST of size 1 or size 0 - I.e. the empty list, return nil."
  (inj0h:zip-pair-impl list '()))
;; Tests:
;; (inj0h:zip-pair '(:foo "foo" :bar "bar"))
;; (inj0h:zip-pair '(:foo "foo" :bar))
;; (inj0h:zip-pair '(:foo))
;; (inj0h:zip-pair '())

(defun inj0h:zip-pair-impl (list acc)
  "The implementation details for `inj0h:zip-pair'."
  (let ((first (car list))
        (next (cadr list))
        (rest (cddr list)))
    (if (and first next)
        (progn
          (push (cons first next) acc)
          (inj0h:zip-pair-impl rest acc))
      (nreverse acc))))

;;; 03. User Macros:

;; TODO(): Refactor error handling for bad argument values, E.g. a mode that
;;         doesn't exist
(defmacro inj0h:setup-mode (&rest args)
  "Setup an extant mode with the following parameters:

- :mode (required) = Name of an extant mode
- :hook            = Name of the hook for :mode; do not provide unless the
                     mode's hook name differs from the \"modename-hook\" naming
                     convention
- :assf            = List of filenames or extensions to associate with :mode
- :assf-mode       = Override :mode with this value for :assf
- :assm            = List of other modes to load with :mode
- :conf (required) = List of expressions to evaluate when first loading :mode

These arguments will evaluate lazily - After loading the mode. Do not use this
macro to setup modes that Emacs loads by default.

E.g.

(inj0h:setup-mode
 :mode text-mode
 :assf (\"COMMIT_EDITMSG\")
 :assm (flyspell-mode)
 :conf ((setq-local fill-column 80)))
"
  (let* ((parsed-list (inj0h:zip-pair args))
         (labels (mapcar '(lambda (pl) (symbol-name (car pl))) parsed-list))
         (required-labels '(":mode" ":conf"))
         (valid-labels (append required-labels
                               '(":hook" ":assf" ":assf-mode" ":assm")))
         (invalid-args nil)
         (incomplete-args nil))

    ;; Check that arguments are valid:
    (dolist (l labels)
      (when (not (member l valid-labels))
        (setq invalid-args t)))

    ;; Check required arguments:
    (dolist (rl required-labels)
      (when (not (member rl labels))
        (setq incomplete-args t)))

    `(let* ((mode ',(inj0h:get-from-list-else parsed-list ":mode" nil))
            (mode-name (if mode (symbol-name mode) "INVALID_MODE")))
       (cond (,invalid-args
              (message
               "Found invalid arguments labels while calling inj0h:setup-mode
               for %s!" mode-name))
             (,incomplete-args
              (message
               "Missing required argument labels while calling inj0h:setup-mode
               for %s!" mode-name))
             (t
              ;; TODO(): Move this to the end of the macro?
              (message
               "Calling inj0h:setup-mode for %s" mode-name)
              (let* ((hook ',(inj0h:get-from-list-else parsed-list ":hook" nil))
                     (assf ',(inj0h:get-from-list-else parsed-list ":assf" nil))
                     (assf-mode
                      ',(inj0h:get-from-list-else parsed-list ":assf-mode" nil))
                     (assm ',(inj0h:get-from-list-else parsed-list ":assm" nil))
                     (conf (cons 'lambda (cons '()
                                               ',(inj0h:get-from-list-else
                                                  parsed-list ":conf" nil)))))
                (with-eval-after-load mode
                  (let ((use-hook (if hook
                                      hook
                                    (intern
                                     (concat (symbol-name mode) "-hook")))))
                    (when assf
                      (let* ((use-mode (if assf-mode assf-mode mode))
                             (filetypes
                              (mapcar #'(lambda (x) (cons x use-mode)) assf)))
                        (dolist (ft filetypes)
                          (add-to-list 'auto-mode-alist ft))))

                    (when assm
                      (dolist (md assm) (add-hook use-hook md)))

                    (add-hook use-hook conf)))))))))

;;; 04. Disable:

(setq auto-save-default nil
      bookmark-set-fringe-mark nil
      create-lockfiles nil
      flyspell-duplicate-distance 0 ; Broken on Mac
      inhibit-startup-screen t
      kill-ring-max 1
      make-backup-files nil
      split-width-threshold nil)
(global-hl-line-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; 05. Vanilla Settings:

;; Custom File
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Encoding
;; TODO: Test how to thoroughly set this up to conveniently adapt for both
;;       Windows and Unix
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Font
(set-frame-font "Iosevka-14" nil t) ; Make sure the OS has this installed!

;; Formatting
(setq c-basic-offset inj0h:default-indent
      require-final-newline t
      sentence-end-double-space nil)
(setq-default fill-column inj0h:default-column
              indent-tabs-mode nil
              tab-width inj0h:default-indent)

;; Keybindings
(inj0h:add-local-vi-bindings
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
      scroll-bar-adjust-thumb-portion nil ; Only works on X11
      scroll-preserve-screen-position nil)

;; Windows/Frames
(setq initial-frame-alist '((width . 90) (height . 35)))

;; Render non-focused frames transparent - I.e. when setting the alpha
;; (transparency level), the first and second numbers indicate focused and
;; unfocused transparency respectively. 100 alpha means opaque.
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

;;; 06. Vanilla Packages:

(setq blink-cursor-blinks 30)
(blink-cursor-mode 1)

(setq column-number-mode t)
(setq-default column-number-indicator-zero-based nil)

(require 'ansi-color)
(setq compilation-scroll-output 1)
(add-hook 'compilation-filter-hook #'inj0h:compile-with-color)

(setq dabbrev-case-distinction nil
      dabbrev-case-fold-search t
      dabbrev-case-replace nil)

(delete-selection-mode t)

(setq dired-listing-switches "-alo")

(setq display-line-numbers-grow-only t)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
;; Return to the buffer where the user called ediff-buffers
(add-hook 'ediff-quit-hook '(lambda ()
                              (other-window 1)
                              (delete-other-windows)))

;; Flyspell/Ispell
(setq flyspell-default-dictionary "en_US")
(cond ((equal system-type 'gnu/linux)
       (setq ispell-program-name "/usr/bin/aspell"))
      ((equal system-type 'darwin)
       (setq ispell-program-name "/usr/local/bin/aspell")))

(global-auto-revert-mode 1)

(setq ibuffer-default-sorting-mode 'filename/process)

(setq ido-auto-merge-work-directories-length -1
      ido-case-fold t
      ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

(setq inj0h:isearch-mode-keybindings
      '(("<up>"   . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)))
(add-hook 'isearch-mode-hook
          '(lambda ()
             (dolist (bindings inj0h:isearch-mode-keybindings)
               (define-key isearch-mode-map
                 (kbd (car bindings)) (cdr bindings)))))

(setq-default ; Mostly to remove vc stuff
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   evil-mode-line-tag
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

(require 'server)
(unless (server-running-p) (server-start))

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Subword Mode
(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'tetris-mode-hook
          '(lambda ()
             (inj0h:create-keybindings
              tetris-mode-map
              '(("," . tetris-rotate-prev)
                ("a" . tetris-move-left)
                ("o" . tetris-move-down)
                ("e" . tetris-move-right)))))

(setq tramp-default-method "ssh")

(setq visible-bell 1)

;; Whitespace Mode
(setq-default whitespace-line-column nil) ; Use fill-column value
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; 07. Vanilla Programming Language Packages:

(add-hook 'java-mode-hook '(lambda ()
                             (let ((java-indent 2)) ; Blame Google
                               (setq-local c-basic-offset java-indent
                                           fill-column 100
                                           evil-shift-width java-indent
                                           tab-width java-indent))))

(add-hook 'js-mode-hook '(lambda ()
                           (let ((js-indent inj0h:default-indent))
                             (setq-local evil-shift-width js-indent
                                         js-indent-level js-indent
                                         tab-width js-indent))))

(add-hook 'latex-mode-hook
          '(lambda () (setq-local fill-column inj0h:default-column)))
(add-hook 'latex-mode-hook 'flyspell-mode)

(add-hook 'nxml-mode-hook
          '(lambda ()
             (let ((xml-indent 2))
               (setq nxml-attribute-indent xml-indent
                     nxml-child-indent xml-indent)
               (setq-local evil-shift-width xml-indent
                           tab-width xml-indent))))

(setq sh-indentation inj0h:default-indent)

(inj0h:setup-mode
 :mode text-mode
 :assf ("COMMIT_EDITMSG")
 :assm (flyspell-mode)
 :conf ((setq-local fill-column inj0h:default-column)))

;;; 08. Org Mode:

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
                                    "PAUSED(p!)"
                                    "BLOCKED(b@/!)"
                                    "|"
                                    "DONE(d!)"
                                    "CANCELED(c@/!)"))
      org-use-fast-todo-selection t)
(setq-default org-display-custom-times t)

(add-hook 'org-mode-hook
          '(lambda () (setq-local fill-column inj0h:default-column)))
(add-hook 'org-mode-hook 'org-indent-mode)

(with-eval-after-load 'org-agenda
  (progn
    (setq org-agenda-custom-commands
          `(("A" "Custom Agenda"
             ((todo "ACTIVE\|BLOCKED"
                    ((org-agenda-overriding-header
                      "You Can (Not) Do It\n\nCurrent:")))
              (agenda "" ((org-agenda-block-separator ?-)
                          (org-agenda-overriding-header "\nToday:")
                          (org-agenda-span 1)
                          (org-deadline-warning-days 0)
                          (org-scheduled-past-days 0)
                          (org-agenda-day-face-function
                           (lambda (date) 'org-agenda-date))
                          (org-agenda-format-date "%Y.%m.%d %A")))
              (agenda "" ((org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\nNext Five Days:")
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-start-day "+1d") ; Start after 1 day to avoid overlap with the previous section
                          (org-agenda-span 5)
                          (org-deadline-warning-days 0)
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-format-date "%Y.%m.%d %A")))
              (agenda "" ((org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\nNext Thirty Days:")
                          (org-agenda-time-grid nil)
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-start-day "+6d") ; Start after 5 days to avoid overlap with the previous section
                          (org-agenda-span 30)
                          (org-agenda-show-all-dates nil)
                          (org-deadline-warning-days 0)
                          (org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-format-date "%Y.%m.%d %A")))))))
    (setq org-agenda-files (list org-directory))))

;; NOTE: Creating TODOs doesn't always auto-revert the TODO Org buffer
;;       (Emacs 28.2, macOS 11)
(setq org-capture-templates
      '(("t"
         "File TODO"
         entry
         (file "todos.org")
         "* TODO %?\n** Subtasks [/]\n** Notes\n")))

;;; 09. Package Management:

;; (setq url-proxy-services
;;       '(("http"  . "proxy:port")
;;         ("https" . "proxy:port")))

;; TODO(): Refactor this code so that it correctly installs missing packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (packages '(corfu
                    diminish
                    evil
                    evil-escape
                    go-mode
                    json-mode
                    kuronami-theme
                    markdown-mode
                    ; nix-mode ; (ㅜㅜ)
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

;;; 10. Evil Mode (Non-Vanilla settings begin here):

;; Summon the Editor of the Beast - VI VI VI
;;
;; Keybindings tuned for EN-Dvorak. Don’t change default vi/Vim too much!
;;
;; This configuration uses custom Elisp code to recreate Vim leader keybinding
;; features that third party packages like “Evil Leader” and “General” provide
(require 'evil)
(require 'evil-escape)
(require 'undo-fu)
(evil-mode 1)
(evil-escape-mode t)
(evil-select-search-module 'evil-search-module 'evil-search)
(setq evil-ex-complete-emacs-commands 'never ; Broken
      evil-want-empty-ex-last-command nil)

(define-key evil-insert-state-map (kbd "\C-n")
  '(lambda ()
     (interactive) (dabbrev-completion 1))) ; Search in same Major Mode Buffers
(define-key evil-normal-state-map (kbd "\C-r") 'undo-fu-only-redo)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)

;; Binding the Evil vi style splits keys to Emacs splits prevents a bug with
;; random cursor "jumping"
(define-key evil-normal-state-map (kbd "C-w C-s") 'split-window-below)
(define-key evil-normal-state-map (kbd "C-w s") 'split-window-below)
(define-key evil-normal-state-map (kbd "C-w C-v") 'split-window-right)
(define-key evil-normal-state-map (kbd "C-w v") 'split-window-right)

(inj0h:create-keybindings
 evil-motion-state-map
 '((";"  . evil-ex)
   (":"  . evil-repeat-find-char)
   ("gc" . comment-dwim)
   ("zg" . inj0h:add-word-to-dictionary)))

(with-eval-after-load 'org
  (evil-define-key 'motion org-mode-map (kbd "<tab>") 'org-cycle))

(setq-default evil-escape-key-sequence "hh"
              evil-escape-excluded-states '(normal visual motion)
              evil-escape-delay 0.2)

(define-prefix-command 'inj0h:evil-leader-keymap)

;; Using evil-define-key here will not bind additional mappings from other
;; plugins for some reason, whereas define-key does what we want
(define-key evil-motion-state-map (kbd "SPC") 'inj0h:evil-leader-keymap)

;; TODO(): Refactor these using Evil Mode's leader API
(setq inj0h:evil-leader-bindings
      '(("'" . (lambda ()
                 (interactive) (org-agenda nil "A") (delete-other-windows)))
        ("," . (lambda ()
                 (interactive) (org-capture nil "t") (delete-other-windows)))
        ("." . xref-find-definitions)
        ("p" . occur)
        ("g" . inj0h:grep-from-here)
        ("C" . inj0h:compile)
        ("c" . inj0h:compile-again)
        ("r" . align-regexp)
        ("a" . apropos)
        ("o" . switch-to-buffer)
        ("e" . find-file)
        ("n" . count-words-region)
        ("s" . sort-lines)
        (";" . server-edit)
        ("b" . ibuffer)
        ("w" . whitespace-mode)))

(inj0h:create-keybindings inj0h:evil-leader-keymap inj0h:evil-leader-bindings)

;; The following keybindings only affect the particular mode

(inj0h:create-leader-local-keybindings
 "SPC"
 'compilation-mode-hook
 'inj0h:evil-leader-compilation-keymap
 (append inj0h:evil-leader-bindings
         '(("mk" . kill-compilation)
           ("mr" . recompile))))

(inj0h:create-leader-local-keybindings
 "SPC"
 'dired-mode-hook
 'inj0h:evil-leader-dired-keymap
 (append inj0h:evil-leader-bindings
         '(("mw" . wdired-change-to-wdired-mode))))

(add-hook 'ibuffer-mode-hook
          '(lambda () (local-set-key (kbd "SPC") 'inj0h:evil-leader-keymap)))

(with-eval-after-load 'org
  (inj0h:create-leader-evil-keybindings
   "SPC"
   org-mode-map
   'motion
   'inj0h:evil-leader-org-keymap
   (append inj0h:evil-leader-bindings
           '(("ma" . org-archive-subtree)
             ("mc" . org-copy-subtree)
             ("mD" . (lambda () (interactive) (org-deadline '(4))))
             ("md" . org-deadline)
             ("mi" . org-insert-heading)
             ("mp" . org-paste-subtree)
             ("mS" . (lambda () (interactive) (org-schedule '(4))))
             ("ms" . org-schedule)
             ("mx" . org-cut-subtree)))))

;;; 11. Non-Vanilla Packages:

(require 'corfu)
(setq corfu-auto nil
      corfu-cycle t
      corfu-excluded-modes '(bookmark-bmenu-mode
                             compilation-mode
                             dired-mode
                             ibuffer-mode))
(define-key corfu-map (kbd "M-t") 'corfu-previous)
(global-corfu-mode)

(require 'diminish)
(diminish 'evil-escape-mode)
(with-eval-after-load 'org-indent (diminish 'org-indent-mode))
(with-eval-after-load 'subword (diminish 'subword-mode))

(load-theme 'kuronami t)

(add-hook 'org-mode-hook 'org-bullets-mode)

;;; 12. Non-Vanilla Programming Language Packages:

(inj0h:setup-mode
 :mode go-mode
 :conf ((let ((go-indent 2))
          (setq-local evil-shift-width go-indent
                      tab-width go-indent))))

(inj0h:setup-mode
 :mode json-mode
 :assf ("\\.eslintrc\\'" "\\.prettierrc\\'")
 :conf ((let ((json-indent 2))
          (setq-local evil-shift-width json-indent
                      js-indent-level json-indent
                      tab-width json-indent))))

(inj0h:setup-mode
 :mode markdown-mode
 :assf-mode gfm-mode
 :assf ("\\.md\\'" )
 :assm (flyspell-mode)
 :conf ((cond ((string-equal system-type "gnu/linux")
               (setq markdown-command "/usr/bin/pandoc"))
              ((string-equal system-type "darwin")
               (setq markdown-command "/usr/local/bin/pandoc")))
        (setq-local fill-column inj0h:default-column)))

(inj0h:setup-mode
 :mode rust-mode
 :conf ((setq-local fill-column 99)))


(inj0h:setup-mode
 :mode yaml-mode
 :conf ((let ((yaml-indent 2))
          (setq yaml-indent-offset yaml-indent)
          (setq-local evil-shift-width yaml-indent
                      tab-width yaml-indent))))

(inj0h:setup-mode
 :mode zig-mode
 :conf ((setq zig-format-on-save nil)
        (setq-local fill-column 100)))
