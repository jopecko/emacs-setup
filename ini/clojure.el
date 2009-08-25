;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-clojure.el
;;; Purpose: Setup for Clojure mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Clojure

;; dump message into *msg* buffer to remind to setq

(when (file-directory-p clj-dir)
  (add-to-list 'load-path (concat clj-dir "/clojure-mode"))
  (add-to-list 'load-path (concat clj-dir "/swank-clojure"))

  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (setq swank-clojure-jar-path (concat clj-dir "/clojure/clojure.jar"))
  (require 'swank-clojure-autoload)

  (unless (boundp 'swank-clojure-extra-classpaths)
    (setq swank-clojure-extra-classpaths nil))
  (add-to-list 'swank-clojure-extra-classpaths (concat clj-dir "/clojure-contrib/src"))

  ;; provides support for running Clojure tests from within Emacs buffers
  ;; and seeing the results displayed inline.
  (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
  (autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
  (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
  (add-hook 'clojure-mode-hook
            (lambda () (save-excursion
                         (goto-char (point-min))
                         (if (search-forward "(deftest" nil t)
                             (clojure-test-mode)))))

  ;; Clojure Paredit integration
  (add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

  ;; http://technomancy.us/126
  ;; prompt for a project root and then launch a SLIME session configured with
  ;; a classpath for that project
  (defun clojure-project (path)
    "Setup classpaths for a clojure project and starts a new SLIME session."
    (interactive (list
                  (ido-read-directory-name
                   "Project root: "
                   (locate-dominating-file default-directory "pom.xml"))))
    (when (get-buffer "*inferior-lisp*")
      (kill-buffer "*inferior-lisp*"))
    (defvar swank-clojure-extra-vm-args nil)
    (defvar slime-lisp-implementations nil)
    (add-to-list 'swank-clojure-extra-vm-args
                 (format "-Dclojure.compile.path=%s"
                         (expand-file-name "target/classes/" path)))
    (setq swank-clojure-binary nil
          swank-clojure-jar-path (expand-file-name "target/dependency/" path)
          swank-clojure-extra-classpaths
          (append (mapcar (lambda (d) (expand-file-name d path))
                          '("src/" "target/classes/" "test/"))
                  (let ((lib (expand-file-name "lib" path)))
                    (if (file-exists-p lib)
                        (directory-files lib t ".jar$"))))
          slime-lisp-implementations
          (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
                (remove-if #'(lambda (x) (eq (car x) 'clojure))
                           slime-lisp-implementations)))
    (save-window-excursion
      (slime))))
