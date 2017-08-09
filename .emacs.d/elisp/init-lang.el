;; ------------------------------------------------------------------------------
;; Filename: init-lang.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp language configuration module
;; 
;; ------------------------------------------------------------------------------


;; aspell: supporting human languages.
(setq ispell-program-name "/usr/local/bin/aspell")


;; Swift
(use-package swift-mode 
  :ensure t
  :config
  )


;; Flycheck linter
(use-package flycheck 
  :ensure t
  :config

  (use-package flycheck-swift3
    :ensure t
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))))


(provide 'init-lang)
