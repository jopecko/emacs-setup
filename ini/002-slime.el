;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-slime.el
;;; Purpose: Setup for Slime mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Slime

(when (file-directory-p slime-dir)
  (add-to-list 'load-path slime-dir)
  ;; (add-to-list 'load-path (concat slime-dir "/contrib"))

  (require 'slime-autoloads)
  ;; (slime-setup '(slime-fancy))

  (defun slime-smart-quit ()
    (interactive)
    (when (slime-connected-p)
      (if (equal (slime-machine-instance) "my.workstation")
          (slime-quit-lisp)
        (slime-disconnect)))
    (slime-kill-all-buffers))

  ;(define-key slime-repl-mode-map [(hyper q)] 'slime-smart-quit)

  ;; contrib functionality is loaded after SLIME has been loaded
  ;; http://bc.tech.coop/blog/070927.html
  (eval-after-load "slime"
    '(progn
       (add-to-list 'load-path (concat slime-dir "contrib"))
       ;; (slime-setup '(slime-fancy slime-asdf slime-banner))
       (slime-setup '(slime-fancy slime-banner))
       (setq slime-complete-symbol*-fancy t)
       (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

  ;; (add-hook 'kill-emacs-hook 'slime-smart-quit)
)
