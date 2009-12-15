;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-rails
;;; Purpose: Setups for the ido package
;;; ==================================================================

;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
(setq ido-use-filename-at-point t) ;; prefer file names near point

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable_recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project files: "
                         (tags-table-files)
                         nil t)))

(defun ffp () (interactive) (ido-find-file-in-tag-files))
(global-set-key "\C-c\C-f" 'ido-find-file-in-tag-files)
