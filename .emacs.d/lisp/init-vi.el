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
    (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
    (define-key evil-insert-state-map (kbd "TAB") 'yas-insert-snippet)))

(defun my-evil-leader-settings ()
  "Configure evil leader."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "`" 'delete-other-windows
    ;; "2" (kbd "@@") ; This keybinding doesn't quite work
    "-" (kbd "\C-b\S-m")
    "=" (kbd "\C-f\S-m")
    "w" 'helm-buffers-list
    "i" (lambda (n) (interactive "p") (evil-open-above n) (evil-force-normal-state))
    "o" 'helm-find-files
    "s" 'other-window
    "S" 'split-window-below
    "f" 'helm-do-ag-buffers
    "k" 'comment-dwim
    "K" 'kill-this-buffer
    "l" 'goto-last-change
    "L" 'linum-relative-toggle
    "c" 'whitespace-mode))

(defun my-keychord-settings ()
  "Configure evil mode"
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (define-key evil-normal-state-map (kbd ":") 'execute-extended-command))

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
    (global-evil-surround-mode))

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode)))

(provide 'init-vi)
