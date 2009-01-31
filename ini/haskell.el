;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    haskell.el
;;; Purpose: Setup for Haskell mode
;;; ==================================================================

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
