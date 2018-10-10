;; Filename: init.el
;; Maintainer: erikorojo
;; License: n/a
;; Comments: Elisp configuration file
;;

;; Manage Lisp files and packages
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Add all Lisp files
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Warning!
;; At the time of writing this comment, use-package and its dependencies,
;; bind-key and diminish, might break when downloading from Melpa.
;;
;; If that problem comes up, just use Melpa stable to install them.
;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Dump all the custom-var-face s*** here.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Source separate configuration modules
(require 'init-ui)
(require 'init-fuzzy)
(require 'init-lang)
(require 'init-vi)
