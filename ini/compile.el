;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-compile
;;; Purpose: Setups for Compilation Mode
;;; ==================================================================

;;; Compilation Mode Setups ==========================================

(load-library "compile")

;;; For some reason, the gnu regex is capturing leading white space.
(setq compilation-error-regexp-alist
      (remq 'gnu compilation-error-regexp-alist))

;; Ruby test unit patterns.
(add-to-list 'compilation-error-regexp-alist
             '("\\([^ \t:\\[]+\\):\\([0-9]+\\):in" 1 2))
(add-to-list 'compilation-error-regexp-alist
             '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))


;; FIC pattern
(add-to-list 'compilation-error-regexp-alist
             '("^\\([^ \t:\\[]+\\):\\([0-9]+\\):" 1 2))

;; SmallEiffel:
;; Line 81 column 13 in GREED_GAME (./greed_game.e) :
(add-to-list 'compilation-error-regexp-alist
             '("Line \\([0-9]+\\) column [0-9]+ in [A-Za-z0-9_]+ (\\(.*\\)) +:" 2 1) )

;; Highlight any line numbers in the compilation error
;; http://lifegoo.pluskid.org/?p=245
(add-to-list 'compilation-mode-font-lock-keywords
	     '("^\\([[:digit:]]+\\) examples?, \\([[:digit:]]+\\) failures?\\(?:, \\([[:digit:]]+\\) pendings?\\)?$"
	       (0 '(face nil message nil help-echo nil mouse-face nil) t)
	       (1 compilation-info-face)
	       (2 (if (string= "0" (match-string 2))
		      compilation-info-face
		    compilation-error-face))
	       (3 compilation-info-face t t)))

;; scroll the compilation buffer result
(setq compilation-scroll-output t)
