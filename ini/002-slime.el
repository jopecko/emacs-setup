;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-slime.el
;;; Purpose: Setup for Slime mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Slime

(when (file-directory-p slime-dir)
  (add-to-list 'load-path slime-dir)
  (add-to-list 'load-path (concat slime-dir "/contrib"))

  (require 'slime)
  (slime-setup '(slime-fancy)))
