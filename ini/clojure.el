;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-clojure.el
;;; Purpose: Setup for Clojure mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Clojure

(setq clj-dir (concat home-dir "lisp/clj"))

(setq clojure-mode-dir (concat clj-dir "clojure-mode"))
(when (file-directory-p clojure-mode-dir)
  (add-to-list 'load-path clojure-mode-dir)
  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  ;;(require 'clojure-auto)
)

;; swank-clojure
(setq swank-clojure-dir (concat clj-dir "swank-clojure"))
(when (file-directory-p swank-clojure-dir)
  (add-to-list 'load-path swank-clojure-dir)
  (setq swank-clojure-binary (concat home-dir "bin/clj"))
  (require 'swank-clojure-autoload))

;(defvar clj-root (concat home-dir "lisp/clj"))
;(setq load-path (append (list (concat clj-root "slime")
;			      (concat clj-root "slime/contrib")
;			      (concat clj-root "clojure-mode")
;			      (concat clj-root "swank-clojure"))
;			load-path))

;(setq swank-clojure-binary "clj")
;(defvar clj-cmd)
;(setenv "CLJ_CMD"
;	(setq clj-cmd
;	      (concat "java "
;		      "-server "
;		      "-Xdebug -runjdwp:transport=dt_socket,server=y,suspend=n,address=8888 "
;		      "clojure.lang.Repl")))



;;; Jim Weirich's setup
;(setq inferior-lisp-program
;                                        ; Path to java implementation
;      (let* ((java-path "java")
                                        ; Extra command-line options
                                        ; to java.
;             (java-options "-Xms100M -Xmx600M")
                                        ; Base directory to Clojure.
                                        ; Change this accordingly.
;             (clojure-path "/usr/local/clojure/")
                                        ; The character between
                                        ; elements of your classpath.
;             (class-path-delimiter ":")
;             (class-path (mapconcat (lambda (s) s)
                                        ; Add other paths to this list
                                        ; if you want to have other
                                        ; things in your classpath.
;                                    (list (concat clojure-path "clojure.jar")
;       (concat clojure-path "clojure-contrib.jar"))
;                                    class-path-delimiter)))
;        (concat java-path
;                " " java-options
;                " -cp " class-path
;                " clojure.lang.Repl")))
 
;; Require clojure-mode to load and associate it to all .clj files.

;(require 'clojure-mode)
