;; Filename: init-lang.el
;; Maintainer: erikorojo
;; License: n/a
;; Comments: Elisp language configuration module
;;

;; Support Human Languages... or at least English.
(cond ((string-equal system-type "gnu/linux")
       (setq ispell-program-name "/usr/bin/aspell"))
      ((string-equal system-type "darwin")
       (setq ispell-program-name "/usr/local/bin/aspell")))

(defun my-rainbow-delimeters-settings ()
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'html-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'javascript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scss-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook #'rainbow-delimiters-mode))

(defun yas-enable-and-reload ()
  "Enable yas-minor-mode for buffer and reload all snips"
  (yas-minor-mode)
  (yas-reload-all))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook '(lambda() (set-fill-column 80)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

;; Colored delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (my-rainbow-delimeters-settings))

;; Flycheck linter
(use-package flycheck
  :ensure t
  :config
  )

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ;; Not sure if this hook does much atm.
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "/usr/local/bin/pandoc")
  (add-hook 'markdown-mode-hook 'flycheck-mode))

(use-package web-mode
  :ensure t
  :config
  )

;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  )

(provide 'init-lang)
