;; ------------------------------------------------------------------------------
;; Filename: init-theme.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp theme (colors, et al) configuration module
;;
;; ------------------------------------------------------------------------------


(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))


(provide 'init-theme)
