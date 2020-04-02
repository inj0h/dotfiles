;; -*- lexical-binding: t -*-
;;
;; Filename: doodads.el
;; Note:     Some homebrewed Emacs Lisp.

(defun foo/add-word-to-dictionary ()
  "Add the word-at-point to aspell's dictionary."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save
                           nil
                           (car word)
                           current-location
                           (cadr word)
                           (caddr word)
                           current-location))))

(defun foo/goto-previous-buffer ()
  "Return to the previously visited buffer. This function is
     interactive."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun foo/kill-filepath ()
  "Copy the current buffer filename with path to clipboard. This
     function is interactive."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer filepath '%s' to clipboard."
               filepath))))

(defun foo/kill-line-to-beginning-of-line ()
  "Kill all text from point to the beginning of the line."
  (interactive)
  (kill-line 0))
