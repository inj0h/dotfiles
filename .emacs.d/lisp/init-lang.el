;; ------------------------------------------------------------------------------
;; Filename: init-lang.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp language configuration module
;;
;; ------------------------------------------------------------------------------


;; aspell: supporting human languages.
(setq ispell-program-name "/usr/local/bin/aspell")

(defun my-rainbow-delimeters-settings ()
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode))

;; Pair 'em up
(electric-pair-mode t)
(setq electric-pair-pairs '((?\" . ?\")
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\{ . ?\})))

;; Colored delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (my-rainbow-delimeters-settings))

;; Flycheck linter
(use-package flycheck
  :ensure t
  :config

  (use-package flycheck-swift3
    :ensure t
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ; Not sure if this hook does much atm.
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Swift
(use-package swift-mode
  :ensure t
  :config
  (add-hook 'swift-mode-hook 'flycheck-mode))

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  )

(provide 'init-lang)
