;; Filename: init-fuzzy.el
;; Maintainer: erikorojo
;; License: n/a
;; Comments: Elisp (mostly) helm configuration module
;;

(defun my-helm-settings ()
  "General helm settings."
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-idle-delay 0.0)
  (setq helm-input-idle-delay 0.01)
  (setq helm-autoresize-max-height 40)
  (helm-autoresize-mode 1)

  ;;(defadvice helm-display-mode-line (after undisplay-header activate)
  ;; (setq header-line-format nil))
  )

(defun my-helm-keybindings ()
  "Helm keybindings, etc."
  (define-key helm-map [tab] 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") 'helm-find-files-down-last-level)
  (define-key helm-map (kbd "C-k") 'helm-find-files-up-one-level)
  (define-key helm-map (kbd "C-o") 'helm-select-action))

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (my-helm-settings)
  (my-helm-keybindings)

  (use-package helm-ag
    :ensure t
    :config
    ))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0))

(provide 'init-fuzzy)
