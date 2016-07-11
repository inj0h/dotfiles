;; Filename: load-packages.el
;; Description: package checker for configuration file. 
;; -----------------------------------------------------------------------------

;; Probably need Common Lisp macro, "for loop"
(require 'cl)

;; Check that we installed the following packages.
(defvar required-packages
  '(goto-chg
    goto-last-change
    undo-tree
    evil
    evil-leader
    evil-mc
    evil-surround
    evil-tabs
    flycheck
    gruvbox-theme
    key-chord)
  "Packages that require installation at launch.")

(defun package-not-installed ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (package-not-installed)
  ;; Check for new packages versions.
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " Done.")
  ;; Install missing packages.
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
