;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-slime.el
;;; Purpose: Setup for Slime mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Slime

(setq slime-dir (concat home-dir "lisp/slime"))
(when (file-directory-p slime-dir)
  (add-to-list 'load-path slime-dir)
  (require 'slime)
  (slime-setup))

;; jump to slime when a lisp file is opened
;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;(setq inferior-lisp-program ""
;      lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;      common-lisp-hyperspec-root ""
;      slime-startup-animation t)

