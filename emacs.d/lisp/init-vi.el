;; Filename: init-vi.el
;; Maintainer: erikorojo
;; License: n/a
;; Comments: Elisp vi configuration module
;;

(defun my-evil-settings ()
  "Vi keybindings, etc."
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)
    (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
    (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
    (define-key evil-motion-state-map (kbd "s-e") 'eval-last-sexp)
    (define-key evil-insert-state-map (kbd "C-y") 'yas-insert-snippet)))

(defun my-toggle-linenumbers ()
  "Toggle between regular/relative/no line numbers. Assume Emacs defaults to no
   line numbers."
  (interactive)
  (cond ((and (not (bound-and-true-p linum-mode))
              (not (bound-and-true-p linum-relative-mode)))
         (linum-mode))
        ((and (bound-and-true-p linum-mode)
              (not (bound-and-true-p linum-relative-mode)))
         (linum-relative-mode))
        ((and (bound-and-true-p linum-mode)
              (bound-and-true-p linum-relative-mode))
         (setq linum-relative-mode nil)
         (linum-relative-off))))

(defun my-evil-leader-settings ()
  "Configure evil leader."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "`" 'delete-other-windows
    "2" (kbd "@@")
    "w" 'helm-buffers-list
    "i" (lambda (n) (interactive "p")
          (evil-open-above n) (evil-force-normal-state))
    "o" 'helm-find-files
    "s" 'other-window
    "S" 'split-window-below
    "f" 'helm-do-ag-buffers
    "k" 'comment-dwim
    "K" 'kill-this-buffer
    "l" 'goto-last-change
    "L" 'my-toggle-linenumbers
    "c" 'whitespace-mode))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-in-single-undo t)
  (setq evil-undo-list-pointer t)
  (setq evil-want-fine-undo t)
  (my-evil-settings)

  (use-package evil-escape
    :ensure t
    :config
    (evil-escape-mode t)
    (setq-default evil-escape-key-sequence "hh")
    (setq-default evil-escape-delay 0.25))

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
