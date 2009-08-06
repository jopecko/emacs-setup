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
  ;(setq swank-clojure-binary (concat bin-dir "clj"))
  (setq swank-clojure-jar-path (concat clj-dir "/clojure/clojure.jar"))
  (require 'swank-clojure-autoload)

  (unless (boundp 'swank-clojure-extra-classpaths)
    (setq swank-clojure-extra-classpaths nil))
  (add-to-list 'swank-clojure-extra-classpaths (concat clj-dir "/clojure-contrib/src")))
