;; Filename: emacs
;; Note:     Main Emacs Lisp configuration file.
;;

;; Manage Lisp files and packages
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list
 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load local-only settings file if it exists on disk, and don't throw a
;; warning if it doesn't.
(load "~/dotfiles/emacs.d/prelude" 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Before getting the other things (at least for now).
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; Mac Settings.

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'control
        mac-option-modifier  'meta
        mac-control-modifier 'control))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (string-equal system-type "darwin")
    (exec-path-from-shell-initialize)))

;; Dump all the custom-var-face s*** here.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Just like the other editors.
(global-auto-revert-mode 1)

;; And just like on X11.
(setq mouse-drag-copy-region 1)

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
      me/default-column-limit 72)
(setq-default column-number-indicator-zero-based nil)
(setq-default fill-column me/default-column-limit)
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

;; Eval camelCase as two words
(add-hook 'prog-mode-hook 'subword-mode)

;; Eval sentences
(setq sentence-end-double-space nil)

;; Whitespace
(setq-default whitespace-line-column me/default-column-limit)

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
(add-hook 'minibuffer-setup-hook '(lambda ()
                                    (setq truncate-lines nil)))

;; Keybindings

(dolist (me/emacs-movement-bindings
         '(("M-."   . repeat)))

  (global-set-key (kbd (car me/emacs-movement-bindings))
                  (cdr me/emacs-movement-bindings)))

(define-prefix-command 'me/bindings-map)
(global-set-key (kbd "S-SPC") 'me/bindings-map)
(dolist (me/emacs-action-bindings
         '(;; Buffer/Window
           ("bK"  . kill-buffer-and-window)
           ("bO"  . ido-switch-buffer-other-window)
           ("bk"  . ido-kill-buffer)
           ("bo"  . ido-switch-buffer)
           ("bp"  . me/goto-previous-buffer)
           ("bs"  . save-buffer)

           ;; Dired
           ("dd"  . dired)
           ("dw"  . wdired-change-to-wdired-mode)

           ;; File
           ("f."  . me/kill-filepath)
           ("fB"  . bookmark-set)
           ("fF"  . find-file-other-window)
           ("fL"  . find-file-literally-at-point)
           ("fb"  . bookmark-bmenu-list)
           ("ff"  . ido-find-file)
           ("fl"  . find-file-literally)
           ("fp"  . find-file-at-point)

           ;; (Ma)Git
           ("gbb" . magit-branch)
           ("gbn" . magit-branch-and-checkout)
           ("gbs" . magit-checkout)
           ("glb" . magit-blame)
           ("glc" . magit-blame-copy-hash)
           ("glg" . magit-show-commit)
           ("glq" . magit-blame-quit)
           ("gp"  . magit-push)
           ("gs"  . magit-status)
           ("gul" . magit-pull-from-upstream)
           ("guu" . magit-push-current-to-upstream)

           ;; Project
           ("pa"  . projectile-add-known-project)
           ("pf"  . projectile-find-file)
           ("pr"  . projectile-remove-known-project)
           ("ps"  . projectile-switch-project)

           ;; Quitting
           ("Q"   . save-buffers-kill-emacs)

           ;; Text
           ("tc"  . goto-last-change)
           ("td"  . me/add-word-to-dictionary)
           ("tfp" . fill-paragraph)
           ("tfr" . fill-region)
           ("tlc" . count-words-region)
           ("tll" . display-line-numbers-mode)
           ("tls" . sort-lines)
           ("tra" . query-replace)
           ("trr" . replace-regexp)
           ("trs" . replace-string)
           ("ts"  . deadgrep)
           ("tw"  . whitespace-mode)

           ;; Windowing
           ("w1"  . delete-other-windows)
           ("wd"  . delete-window)
           ("wrm" . recenter)
           ("wrt" . me/recenter-window-top)
           ("ww"  . other-window)))

  (define-key me/bindings-map
    (kbd (car me/emacs-action-bindings))
    (cdr me/emacs-action-bindings)))

;; Turn off lockfiles.
(setq create-lockfiles nil)

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;;
;; Start Utility configuration
;;

(defun me/goto-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Ido
(setq ido-enable-flex-matching t
      ido-case-fold t
      ido-everywhere t)
(ido-mode 1)

;; Dired
(defun me/dired-bindings ()
  (local-set-key (kbd "S-SPC") 'me/bindings-map))

(add-hook 'dired-mode-hook 'me/dired-bindings)

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . execute-extended-command)))

(use-package deadgrep
  :ensure t
  :bind (:map deadgrep-mode-map
              ("q"     . kill-buffer-and-window)
              ("RET"   . deadgrep-visit-result-other-window)
              ("S-SPC" . 'me/bindings-map)))

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
              ("S-SPC" . 'me/bindings-map))
  :config
  (add-hook 'magit-status-mode-hook
            '(lambda () (setq magit-diff-refine-hunk t))))

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
    (define-key company-active-map (kbd "C-p")
      #'company-select-previous)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<"     . mc/mark-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-/"     . mc/skip-to-next-like-this)
         ("C-:"     . mc/skip-to-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-c c a" . mc/edit-beginnings-of-lines)
         ("C-c c c" . mc/edit-lines)
         ("C-c c e" . mc/edit-ends-of-lines)
         ("C-c c *" . mc/mark-all-like-this)
         ("C-c c r" . set-rectangular-region-anchor)))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.1
        which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode)

  (dolist (me/which-key-labels
    '(("S-SPC b"   . "buffer")
      ("S-SPC d"   . "dired")
      ("S-SPC f"   . "files")
      ("S-SPC g"   . "magit")
      ("S-SPC p"   . "projectile")
      ("S-SPC t"   . "text")
      ("S-SPC t f" . "format")
      ("S-SPC t l" . "line")
      ("S-SPC t r" . "replace")
      ("S-SPC w"   . "window")))

    (which-key-declare-prefixes (car me/which-key-labels)
      (cdr me/which-key-labels))))

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

;; Text
(add-hook 'text-mode-hook
          '(lambda () (set-fill-column me/default-column-limit)))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Colored delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'css-mode-hook        #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook    #'rainbow-delimiters-mode)
  (add-hook 'html-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook         #'rainbow-delimiters-mode)
  (add-hook 'scss-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'sh-mode-hook         #'rainbow-delimiters-mode))

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
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))

(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (set-fill-column me/default-column-limit)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

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
        me/git-commit-mode-column-limit 72)

  (add-hook 'git-commit-mode-hook
            '(lambda ()
               (setq-local whitespace-line-column
                           me/git-commit-mode-column-limit)))
  (add-hook 'git-commit-mode-hook
            '(lambda ()
               (set-fill-column me/git-commit-mode-column-limit)))
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

  (add-hook 'markdown-mode-hook '(lambda ()
                                   (setq-local truncate-lines t)))
  (add-hook 'markdown-mode-hook 'flycheck-mode)
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill))

;; Org
(use-package org
  :config
  (setq org-enforce-todo-dependencies t
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

  (add-to-list 'org-src-lang-modes '("haskell". haskell))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-bullets-mode)))

;; Shell
(add-to-list 'auto-mode-alist '("bash_profile" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))

;;
;; Etc
;;

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

(defun me/recenter-window-top ()
   "Recenter the current line to the top of the window."
   (interactive)
   (set-window-start (selected-window) (point)))

;;
;; End Language configuration
;;

;;
;; Server
;;

(require 'server)
(unless (server-running-p)
  (server-start))

;; Load local-only settings file after reading the main init file,
;; i.e. useful when you need to override variables, etc.
;;
;; Don't throw a warning if it doesn't exist on disk.
(load "~/dotfiles/emacs.d/coda" 1)
