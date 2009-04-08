;;; GNU-Emacs Initialization File
;;; based off of Jim Weirich's initialization file

;; Some file locations are relative to the HOME or the BIN directory
(defvar home-dir)
(setq home-dir (concat (expand-file-name "~") "/"))
(defvar bin-dir (concat home-dir "bin/"))

(setq elisp-directory (concat home-dir ".elisp/"))
(load (concat elisp-directory "load-ini.el"))

;; load any customizations for the current machine
(setq dot-emacs-dot-d (concat home-dir ".emacs.d/"))
(when (file-exists-p dot-emacs-dot-d)
       (add-to-load-path dot-emacs-dot-d)
       (let ((local-el (concat dot-emacs-dot-d "local.el")))
         (when (file-readable-p local-el)
           (load local-el))))

;; Keep custom variables, (M-x customize*), in their own file
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(cond ((< emacs-major-version 21)
       ;; Emacs 20 customization
       (setq custom-file (concat elisp-directory "emacs-custom-20.el")))
      ((and (= emacs-major-version 21) (< emacs-minor-version 4))
       ;; Emacs 21 customization, before version 21.4
       (setq custom-file (concat elisp-directory "emacs-custom-21.el")))
      ((< emacs-major-version 22)
       ;; Emacs 21 customization, before version 21.4
       (setq custom-file (concat elisp-directory "emacs-custom-21.4.el")))
      (t
       ;; Emacs version 22.1 or later
       (setq custom-file (concat elisp-directory "emacs-custom-22.el"))))

;; load the stored settings
(when (file-readable-p custom-file)
  (load custom-file 'noerror))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(let ((fn (expand-file-name "~/.emacs.d/elpa/package.el")))
  (when (file-readable-p fn)
    (load fn)
    (package-initialize)))

