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
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Add all directories within "lisp"
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
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

;; Use use-package
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

;; Config modules
(require 'init-ui)
(require 'init-fuzzy)
(require 'init-lang)
(require 'init-vi)
