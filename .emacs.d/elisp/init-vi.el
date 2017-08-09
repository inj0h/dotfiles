;; ------------------------------------------------------------------------------
;; Filename: init-vi.el
;; Maintainer: erikoelrojo
;; License: n/a
;; Comments: Elisp vi configuration module
;; 
;; ------------------------------------------------------------------------------


(defun my-evil-settings ()
  "Vi keybindings, etc."
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)
    (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)))


(defun my-evil-leader-settings ()
  "Configure evil leader."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "`" 'delete-other-windows
    "2" (kbd "@@")
    "-" (kbd "\C-b\S-m")
    "=" (kbd "\C-f\S-m")
    "w"  'helm-buffers-list
    "i" (lambda (n) (interactive "p") (evil-open-above n) (evil-force-normal-state))
    "o"  'helm-find-files
    "s" 'other-window
    "S" 'split-window-below
    "l" 'goto-last-change
    "L" 'linum-relative-toggle))


(defun my-keychord-settings ()
  "Configure evil mode"
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (define-key evil-normal-state-map (kbd ":") 'execute-extended-command))


(use-package linum-relative
    :ensure t
    :config
    (global-linum-mode t)
    (linum-relative-mode t))

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
    (my-evil-leader-settings))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))


(provide 'init-vi)
