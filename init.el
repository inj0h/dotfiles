;; -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Note:     A minimal Emacs initialization file to bootstrap an Org
;;           configuration using Org Babel.

;; Speed up that init time. For reference.
;; - https://so.nwalsh.com/2020/02/29/dot-emacs
;; - https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;;
(setq package-enable-at-startup nil
      site-run-file nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset the garbage collection to the default value.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000
                                          gc-cons-percentage 0.1)))

;; Load the main config.
(setq find-file-visit-truename t)
(org-babel-load-file "~/.config/emacs/dotemacs.org") ;; TODO: Get bytecomp working
