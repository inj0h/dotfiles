;; Filename: bootstrap.el
;; Note:     Deserialization script to bootstrap a pure Elisp configuration from
;;           an Org-Elisp literate configuration.

(require 'org)

(defun ufun:file-extension-p (filename extension)
  "Return t if FILENAME contains no \".\" and its extension equals EXTENSION.
  Otherwise return nil."
  (let ((split-filename (split-string filename "\\.")))
    (if (and (equal (length split-filename) 2)
             (equal (car (cdr split-filename)) extension))
        t
      nil)))

(defun ufun:tangle-emacs-org-config (filename)
  "Tangle FILENAME file into a \"dotemacs_year-month-day.el\" output file.

This method will print a message accordingly for the following error cases.
0. FILENAME is null or its file does not exist.
1. FILENAME does not have a \".org\" extension.
2. The output file already exists.

Note, it does not return a non-zero exit code to STDOUT when printing these
error messages.

This method assumes that FILENAME contains Elisp code. If it does not, this
method will still produce an output file with the tangled source code."
  (let ((output-filename (concat "dotemacs"
                                 (format-time-string "_%Y%m%d") ".el")))
    (cond ((or (equal nil filename)
               (not (file-exists-p filename)))
           (message "Error: Failed to find file."))
          ((not (ufun:file-extension-p filename "org"))
           (message "Error: File does not have \".org\" extension."))
          ((file-exists-p output-filename)
           (message
            "Error: File %s already exists! Please remove it to continue."
            output-filename))
          (t
           (progn
             (org-babel-tangle-file filename output-filename)
             (message
              "Success! Tangled: %s -> %s" filename output-filename))))))

(ufun:tangle-emacs-org-config (pop argv))
