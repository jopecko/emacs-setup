;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    paredit.el
;;; Purpose: Setup for Paredit mode
;;; ==================================================================

(require 'paredit)

(defun enable-paredit ()
  (paredit-mode +1))

;; enable paredit in slime repl
(add-hook 'slime-repl-mode-hook 'enable-paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit)
(add-hook 'lisp-mode-hook 'enable-paredit)
(add-hook 'inferior-lisp-mode-hook 'enable-paredit)
(add-hook 'clojure-mode-hook 'enable-paredit)
(add-hook 'scheme-mode-hook 'enable-paredit)
