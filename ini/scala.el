;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-scala
;;; Purpose: Setups for Scala Mode
;;; ==================================================================

;;; Scala Mode Setups ================================================

(setq scala-elisp-dir "/usr/local/share/emacs/site-lisp/scala")

(if (file-directory-p scala-elisp-dir)
    (progn
      (add-to-list 'load-path scala-elisp-dir)
      (require 'scala-mode-auto)

      (add-hook 'scala-mode-hook
                '(lambda ()
                   (yas/minor-mode-on)))))
