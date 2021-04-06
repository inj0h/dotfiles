;; -*- lexical-binding: t -*-
;; Filename: bootstrap.el
;; Note:     Initialization script to bootstrap a pure Elisp configuration from
;;           an Org configuration.

(require 'org)

(defun ufun:string-valid-p (string)
  "Return t if string is both non-nil and not blank, i.e. (string-blank-p
string) returns non-nil. Otherwise, return nil."
  (if (and string
           (not (string-blank-p string)))
      t
    nil))

(defun ufun:file-extension-p (file)
  "Return t if exactly two substrings can be created by splitting file at
delimiter '.' and file is a valid string, i.e. (ufun:string-valid-p string)
returns t. Otherwise, return nil."
  (if (and (ufun:string-valid-p file)
           (equal (length (split-string file "\\.")) 2))
      t
    nil))

(defun ufun:org-name-tangle-file (file extension &optional timestamp)
  "Replace the file 'extension' with extension. Note for correct functionality,
this method assumes file has a name such that (ufun:file-extension-p file)
returns t.

If timestamp is non-nil, concatenate a timestamp between file and extension. Use
the format year-month-day-hour-minute-second."
  (when (and (ufun:string-valid-p file)
             (ufun:string-valid-p extension))
    (if timestamp
        (let ((tstamp (format-time-string "_%Y%m%d%H%M%S")))
          (concat (car (split-string file "\\.")) tstamp "." extension))
      (concat (car (split-string file "\\.")) "." extension))))

(defun ufun:config-bootstrap (source-file target-extension)
  "Produce an Elisp file by extracting Elisp source blocks from an Org file.
Note, this method is meant to fit a personal workflow for bootstrapping a pure
Elisp configuration from an Org source file which can then easily become
symlinked."
  (if (and (ufun:string-valid-p source-file)
           (ufun:string-valid-p target-extension)
           (ufun:file-extension-p source-file)
           (file-exists-p source-file))
      (let ((target-file (ufun:org-name-tangle-file source-file target-extension 1)))
        (org-babel-tangle-file source-file target-file))
    (print
     (concat "Error: failed to load file - " source-file))))

(ufun:config-bootstrap "dotemacs.org" "el")
