;; ------------------------------------------------------------------------------
;; Filename: init-lint.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp linter configuration module
;; 
;; ------------------------------------------------------------------------------


;; For aspell
(setq ispell-program-name "/usr/local/bin/aspell")

(use-package flycheck 
  :ensure t
  :config
  )


(provide 'init-lint)
