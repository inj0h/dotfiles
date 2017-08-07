;; ------------------------------------------------------------------------------
;; Filename: init.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp configuration file
;; 
;; ------------------------------------------------------------------------------


;; Package management
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Add all directories within "elisp"
(let ((files (directory-files-and-attributes "~/.emacs.d/elisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Dump all the custom-var-face into one file. 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Sane Defaults.
;; I realize the danger.
(setq auto-save-default nil
      inhibit-startup-screen t
      make-backup-files nil)

;; I realize the danger.
(setq ring-bell-function 'ignore)

;; Config modules
(require 'init-fuzzy)
(require 'init-vi)
(require 'init-lang)
(require 'init-theme)

;; UI settings
;; These are a bit of a mess right now...
(blink-cursor-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-attribute 'default t :background "#32302f")
(set-face-attribute 'fringe t :background "#32302f")
(setq column-number-mode t)
(set-cursor-color "#ff6666")
(add-to-list 'default-frame-alist '(cursor-color . "ff6666"))
(global-hl-line-mode t)
(set-face-background hl-line-face "#393642")
(add-to-list 'default-frame-alist '(font . "Menlo-11"))
