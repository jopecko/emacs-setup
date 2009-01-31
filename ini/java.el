;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    java.el
;;; Purpose: Setups for Java Mode
;;;          http://paste.lisp.org/display/62360
;;; ==================================================================

(defun java-getter-from-region (reg-beg reg-end)
  "Creates a getter for a variable in the region"
  (interactive "r")
  (let* ((var-string (buffer-substring reg-beg reg-end))
         (string-list (split-string var-string " "))
         (var-name (substring (cadr string-list) 0 -1)))
    (newline-and-indent)
    (newline-and-indent)
    (insert (concat "public " (car string-list) " " "get"
                    (capital-first-letter var-name)
                    "() {"))
    (newline-and-indent)
    (insert (concat "return " var-name ";"))
    (newline-and-indent)
    (insert "}")
    (c-indent-command)
    (newline-and-indent)))

(defun java-setter-from-region (reg-start reg-end)
  "Creates a setter for a variable in the region"
  (interactive "r")
  (let* ((var-string (buffer-substring reg-start reg-end))
         (string-list (split-string var-string " "))
         (type-id (car string-list))
         (var-name (substring (cadr string-list) 0 -1))
         (new-var (concat "new" (capital-first-letter var-name))))
    (newline-and-indent)
    (newline-and-indent)
    (insert "public void ")
    (insert (concat "set" (capital-first-letter var-name)))
    (insert (concat "(" type-id " " new-var ") {"))
    (newline-and-indent)
    (insert (concat var-name " = " new-var ";"))
    (newline-and-indent)
    (insert "}")
    (c-indent-command)
    (newline-and-indent)))

(defun java-accessor-from-region ()
  "Creates a setter and getter for a variable in the region"
  (interactive)
  (let ((reg-start (region-beginning)) (reg-end (region-end)))
    (java-getter-from-region reg-start reg-end)
    (java-setter-from-region reg-start reg-end)))

(defun java-make-class-template ()
  "If a class name is in the region then a skeleton class will be formed"
  (interactive)
  (let ((class-name (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat "public class " class-name " {"))
    (newline-and-indent)
    (newline-and-indent)
    (insert "}")
    (c-indent-command)
    (previous-line)
    (c-indent-command)))

(defun java-file-class-template ()
  "Creates a skeleton class from the file name of the current buffer"
  (interactive)
  (let ((class-name
         (file-name-sans-extension
          (file-name-nondirectory (buffer-file-name)))))
    (insert (concat "public class " class-name " {"))
    (newline-and-indent)
    (newline-and-indent)
    (insert "}")
    (c-indent-command)
    (previous-line)
    (c-indent-command)))

(defun java-use-debug-info ()
  (interactive)
  (setq use-debug-info t))

(defvar java-exec-class "")
(defvar java-exec-args "")

(defun java-set-exec-class ()
  (interactive)
  (setq java-exec-class (read-from-minibuffer "Execution Class: "))
  (setq java-exec-args (read-from-minibuffer "Arguments: ")))

(defun java-execute-file ()
  (interactive)
  (start-process-shell-command "Java compilation" "*Java compilation buffer*"  (concat "java " java-exec-args " " java-exec-class)))

(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-c\C-g" 'java-getter-from-region)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-c\C-s" 'java-setter-from-region)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-c\C-a" 'java-accessor-from-region)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-c\C-d" 'java-file-class-template)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\M-f" 'c-forward-into-nomenclature)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\M-b" 'c-backward-into-nomenclature)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-c\C-l" 'java-execute-file)))
(add-hook 'java-mode-hook
	  (lambda () (define-key java-mode-map "\C-c\C-v" 'java-set-exec-class)))
(add-hook 'java-mode-hook
	  (lambda () (setq parens-require-spaces nil)))
