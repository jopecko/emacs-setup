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
  (slime-setup '(slime-fancy))

  (defun slime-smart-quit ()
    (interactive)
    (when (slime-connected-p)
      (if (equal (slime-machine-instance) "my.workstation")
          (slime-quit-lisp)
        (slime-disconnect)))
    (slime-kill-all-buffers))

  ;(define-key slime-repl-mode-map [(hyper q)] 'slime-smart-quit)

  (add-hook 'kill-emacs-hook 'slime-smart-quit))

;; need to find a home for this
(require 'paredit)
(defun lisp-enable-paredit-hook () (paredit-mode 1))
;; enable paredit in slime repl
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
