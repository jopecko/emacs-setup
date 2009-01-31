;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-erlang
;;; Purpose: Setups for Erlang Mode
;;; ==================================================================

;;; Erlang Mode Setups ===============================================


(setq erlang-root-dir "/usr/lib64/erlang")
(setq erlang-man-root-dir (concat erlang-root-dir "/man"))

(if (file-directory-p erlang-root-dir)
    (progn
      (setq load-path (cons (concat erlang-root-dir "/lib/tools-2.6.2/emacs")
                             load-path))
      (setq exec-path (cons (concat erlang-root-dir "/bin") exec-path))
      (require 'erlang-start)))

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
          (lambda ()
          ;; when starting an Erlang shell in Emacs, default in the node name
          (setq inferior-erlang-machine-options '("-sname" "emacs"))
          ;; add Erlang functions to an imenu menu
          (imenu-add-to-menubar "imenu")
          ;; customize keys
          (local-set-key [return] 'newline-and-indent)))

;; Initialize flymake so it will properly handle Erlang's source code.
;; This function will catch source code as its typed into the buffer,
;; and pass code to Erlang script, that will perform checking of the code.
(defun flymake-erlang-init()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (concat bin-dir "eflymake") (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
            '("\\.erl\\'" flymake-erlang-init))

(add-hook 'erlang-mode-hook
          (lambda ()
          ;; enable flymake mode for Erlang
          (flymake-mode t)))



;; This is needed for Distel setup
(let ((distel-dir (concat home-dir"repos/svn-repos/distel/elisp")))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)	
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-*"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
