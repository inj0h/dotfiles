;; ------------------------------------------------------------------------------
;; Filename: init-fuzzy.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp (mostly) helm configuration module
;;
;; ------------------------------------------------------------------------------


(defun my-helm-settings ()
  "General helm settings."
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-idle-delay 0.0)
  (setq helm-input-idle-delay 0.01)
  (helm-autoresize-mode 1)

  ;;(defadvice helm-display-mode-line (after undisplay-header activate)
  ;; (setq header-line-format nil))
  )


(defun my-helm-keybindings ()
  "Helm keybindings, etc."
  (define-key helm-map [tab] 'helm-execute-persistent-action) ; Must use [tab] syntax
  (define-key helm-map (kbd "C-j") 'helm-find-files-down-last-level)
  (define-key helm-map (kbd "C-k") 'helm-find-files-up-one-level)
  (define-key helm-map (kbd "C-o") 'helm-select-action)); <- o = option


(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (my-helm-settings)
  (my-helm-keybindings)

  (use-package helm-ag
    :ensure t
    :diminish
    :config
    ))


(provide 'init-fuzzy)
