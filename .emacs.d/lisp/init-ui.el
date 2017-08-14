;; ------------------------------------------------------------------------------
;; Filename: init-theme.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp UI configuration module
;;
;; ------------------------------------------------------------------------------


(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))


(provide 'init-theme)
