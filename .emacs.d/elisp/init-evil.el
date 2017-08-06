;; Test Evil

(defun my-evil-page-down ()
  "Page down then center buffer"
  (interactive)
  (evil-scroll-page-down)
  (evil-scroll-line-to-center))


(defun my-evil-settings ()
  "Evil keybindings, etc."
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)))


(defun my-evil-leader-settings ()
  "Configure evil leader."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "2" (kbd "@@")
    "=" 'my-evil-page-down
    
    )
  )


(defun my-keychord-settings ()
  "Configure evil mode"
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))
;;(define-key evil-normal-state-map (kbd ":") 'execute-extended-command))


(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (my-evil-settings)

  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.5)
    (my-keychord-settings))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (my-evil-leader-settings)))


(provide 'init-evil)
