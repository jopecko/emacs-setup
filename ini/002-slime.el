;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-slime.el
;;; Purpose: Setup for Slime mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Slime

(add-to-list 'load-path (concat home-dir "lisp/slime/"))
(require 'slime)

(slime-setup)

;; jump to slime when a lisp file is opened
;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;(setq inferior-lisp-program ""
;      lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;      common-lisp-hyperspec-root ""
;      slime-startup-animation t)

