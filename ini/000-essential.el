;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-essential.el
;;; Purpose: Essential Emacs Functions and bindings
;;; ==================================================================

;;; Detect emacs version =============================================

(defun is-emacs-19 ()
  (string-equal (substring emacs-version 0 2) "19"))

(defun is-emacs-19-25 ()
  (string-equal (substring emacs-version 0 5) "19.25"))

(defun is-emacs-20 ()
  (string-equal (substring emacs-version 0 2) "20"))

(defun is-emacs-21 ()
  (string-equal (substring emacs-version 0 2) "21"))

(defun is-emacs-22 ()
  (string-equal (substring emacs-version 0 2) "22"))

(defun is-emacs-23 ()
  (string-equal (substring emacs-version 0 2) "23"))

(defun is-xemacs ()
  (string-match "XEmacs" emacs-version))

(defun is-aquamacs ()
  (boundp 'aquamacs-version))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using? start with these, as it will influence
;; other stuff; inspired by: http://www.steve.at/prg/emacs/emacs.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst win32-p (eq system-type 'windows-nt)
  "Are we running on a Windoze system?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-soft (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if 'require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Location Detection ===============================================
;;; This code is used to detect where emacs is running.  The location
;;; test functions allow customization of the setup file.

(setq jw-site
      (cond ((file-readable-p "/vmlinuz") 'home)
	    ((file-readable-p "/boot/vmlinuz") 'home)
	    (t 'unknown)))

(defun at-home () (eq jw-site 'home))
(defun at-who-knows-where () (eq jw-site 'unknown))

;;; Define autolist ==================================================

(defun make-auto (pattern mode)
  "Add a pattern to the auto-mode alist."
  (let ((ans (assoc pattern auto-mode-alist)))
    (if (and ans (equal mode (cdr ans)))
	(print "Do Nothing")
      (print "Added")
      (setq auto-mode-alist
	    (cons (cons pattern mode) auto-mode-alist)))))

;;; Backspace Rebinding ==============================================

;; The following swaps the functions of the delete and backspace keys

;; (defun fix-backspace ()
;;   (interactive)
;;     (let ((the-table (make-string 128 0)))
;;       (let ((i 0))
;; 	(while (< i 128)
;; 	  (aset the-table i i)
;; 	  (setq i (1+ i))))
;;       ;; Swap ^H and DEL
;;       (aset the-table ?\177 ?\^h)
;;       (aset the-table ?\^h ?\177)
;;       (setq keyboard-translate-table the-table)) )

;; (if (and (not (is-emacs-19))
;; 	 (not (is-xemacs))
;; 	 (not (is-emacs-21))
;; 	 (not (is-emacs-22)))
;;     (fix-backspace))

;; (defun toggle-bs-mode ()
;;   (interactive)
;;   (if keyboard-translate-table
;;       (progn (setq keyboard-translate-table nil)
;; 	    (message "Normal Key Translation"))
;;     (fix-backspace)
;;     (message "C-h / DEL Swapped") ))

;; (global-set-key "\C-x4h" 'toggle-bs-mode)

;;;_____________________________________________________________________
;;; Customized Variables

;; the modeline
;; make sure we know where we are in the buffer
(setq line-number-mode t                  ; show line numbers
      column-number-mode t)               ; show cloumn numbers
(when-available 'size-indication-mode
		(size-indication-mode t)) ; show file size (Emacs 22+)
(display-time-mode t)

;; general settings
;(menu-bar-mode -1)                        ; don't show the menu
(tool-bar-mode -1)                        ; don't show the toolbar
(transient-mark-mode t)                   ; make the current 'selection' visible
(delete-selection-mode t)                 ; delete the selection with a keypress
(setq search-highlight t                  ; highlight when searching...
      query-replace-highlight t)          ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)             ; enable one letter y/n answers to yes/no
(global-font-lock-mode t)                 ; always do syntax highlighting
;(setq font-lock-maximum-decoration t)
(set-language-environment "UTF-8")        ; prefer utf-8 for language settings
(setq visible-bell t)                     ; turn the damn beef off, but make it visible
(when (require-soft 'jit-lock)            ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1))         ; new with emacs21
(setq x-select-enable-clipboard t)        ; copy-paste should work ...
(setq interprogram-paste-function         ; ...with...
      'x-cut-buffer-or-selection-value)   ; ...other X clients

;; don't commit trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default default-indicate-empty-lines t)
;; (setq-default indent-tabs-mode nil)
;; (set-variable default-indicate-empty-lines t)

;; pretty cool; with this we can shift to different 'windows'
;;  use M-<arrow keys>
(when (require-soft 'windmove)
  (windmove-default-keybindings 'meta))

;; don't show startup messages
;(setq inhibit-startup-message t
;      inhibit-startup-echo-area-message t)

;; set frame title / icon title using filename or buffername
;; little trick (based on http://www.emacswiki/.org/cgi-bin/wiki/ShadyTrees)
;; to replace /home/foo with ~
(defun my-title-format ()
  (if buffer-file-name
      (replace-regexp-in-string "\\\\" "/"
				(replace-regexp-in-string (regexp-quote (getenv "HOME")) "~"
							  (convert-standard-filename buffer-file-name)))
    (buffer-name)))
(setq
 frame-title-format '(:eval (my-title-format))
 icon-title-format  '(:eval (concat "emacs:" (my-title-format))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-system (i.e. _not_ console) specific settings
;;
(when (not console-p)

  ;; highlight the current line; set a custom face, so we can
  ;; recognize it from the normal marking (selection)
  ;; don't turn it on globally, only in specific modes (see my-c-mode-hook)
;;   (when-available 'global-hl-line-mode
;; 		  (progn
;; 		    (defface hl-line '((t (:background "Gray")))
;; 		      "Face to use for 'hl-line-face'." :group 'hl-line)
;; 		    (setq hl-line-face 'hl-line)
;; 		    (global-hl-line-mode t))) ; turn it on for all modes by default
  (when-available 'scroll-bar-mode
		  (progn
		    (scroll-bar-mode t)              ; show the scroll bar...
		    (set-scroll-bar-mode 'right)))   ; ...on the right side
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-paren-mode
;; show a subtle blinking of the matching paren (the defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when-available 'show-paren-mode
		(progn
		  (show-paren-mode t)
                  (setq show-paren-delay 0)
                  ;(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'
		  (set-face-background 'show-paren-match-face (face-background 'default))
		  (set-face-foreground 'show-paren-match-face "white")
		  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
                  (set-face-foreground 'show-paren-mismatch-face "red")
                  (set-face-attribute 'show-paren-mismatch-face nil :weight 'extra-bold :underline t :overline nil :slant 'normal)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time/date/calendar stuff
(display-time)
(setq holidays-in-diary-buffer t
      mark-holidays-in-calendar t
      all-christian-calendar-holidays t)
(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format ""
      default-indicate-empty-lines t
      display-time-use-mail-icon t
      display-time-load-average-threshold 20)

(setq calendar-latitude 40.521634)
(setq calendar-longitude -74.862907)
(setq calendar-location-name "Flemington, NJ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (setq rlogin-initially-track-cwd t)	; track dirs in rlogin
;; (setq next-line-add-newlines nil)	; C-n will not add lines
;; (setq require-final-newline t)		; require files end with newline
;; (setq auto-save-default nil)		; don't auto-save (it annoys me)

;; (setq Info-default-directory-list
;;       (cons "~/.elisp/info" Info-default-directory-list))

;(defvar compile-command "rake ")	; set the default make command
;(make-variable-buffer-local 'compile-command)
					; make the compile command buffer local
					; (this allows each buffer to have its
					;  own custom compile command)

;(put 'narrow-to-region 'disabled nil)	; narrow enabled
;(put 'upcase-region 'disabled nil)	; change case enabled
;(put 'eval-expression 'disabled nil)	; allow eval commands


;;; Key Binding ======================================================
;;; The following key bindings are more or less generic.  Bindings for
;;; newly defined functions usually occur in the file defining the
;;; binding.

;; (global-set-key "\M-g" 'goto-line)	; goto a line position

;; (global-set-key "\C-c " 'shell)		; select a shell
;; (global-set-key "\C-c\C-r" 'shell-resync-dirs) ; resync shell with current dir
;; (global-set-key "\C-cf" 'auto-fill-mode) ; toggle fill mode

;; (global-set-key "\C-x\C-m" 'compile)	; do the compile command
;; (global-set-key "\C-x\C-n" 'next-error)	; goto next compile error
;; (global-set-key "\C-x " 'big-shell)	; select a full screen shell

;; (global-set-key "\C-z" 'scroll-down)	; I *hate* suspend bound on this key

;; (global-set-key "\C-c>" 'tags-reset-tags-tables)

;; (setq shell-dirstack-query "resync_dirs")

;;; The following are only done in emacs-19 --------------------------

;; (if (is-emacs-19)
;;     (progn
;;       (global-set-key (if (is-xemacs) [(shift backspace)] [S-backspace]) "\C-?")
;;       (global-set-key [f1] "\C-h")
;;       (global-set-key [f4] 'call-last-kbd-macro)
;;       (global-set-key (if (is-xemacs) [(shift f4)] [S-f4]) 'name-last-kbd-macrob)
;;       ))

;; (require 'compile)
;; (setq bad-re "\\([^ :]+\\):\\([0-9]+\\):in")

(require 'flymake)

;
; bind F5 to refresh file
(defun refresh-file()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f5] 'refresh-file)
