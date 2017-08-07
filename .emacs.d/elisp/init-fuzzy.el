;; ------------------------------------------------------------------------------
;; Filename: init-fuzzy.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp helm configuration module
;; 
;; ------------------------------------------------------------------------------


(use-package helm 
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq helm-idle-delay 0.0)
  (setq helm-input-idle-delay 0.01)) 


(provide 'init-fuzzy)
