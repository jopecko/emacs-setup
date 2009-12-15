;;; ==================================================================
;;; Author:  Joe O'Pecko
;;; File:    ini-org
;;; Purpose: Setups for Org Mode
;;; ==================================================================

;; http://orgmode.org
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)

;; orgstruct++-mode is enabled in Gnus message buffers to aid in creating
;; structured email messages.
;; (setq message-mode-hook
;;       (quote (orgstruct++-mode
;;               (lambda nil (setq fill-column 72) (flyspell-mode 1))
;;               turn-on-auto-fill
;;               bbdb-define-all-aliases)))

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode to spell check everywhere
            ;;(flyspell-mode 1)
            ))

;;  (setq org-todo-keywords
;;        '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
;;          (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
;;          (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
;;          (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
              (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("STARTED" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("QUOTE" :foreground "red" :weight bold)
        ("QUOTED" :foreground "magenta" :weight bold)
        ("APPROVED" :foreground "forest green" :weight bold)
        ("EXPIRED" :foreground "forest green" :weight bold)
        ("REJECTED" :foreground "forest green" :weight bold)
        ("OPEN" :foreground "blue" :weight bold)))

;; Fast todo selection allows changing from any task todo state to any other
;; state directly by selecting the appropriate key from the fast todo selection
;; key menu. This is a great feature!
;;
;; Changing a task state is done with
;; C-c C-t KEY
;; where KEY is the appropriate fast todo state selection key as defined in
;; org-todo-keywords.
(setq org-use-fast-todo-selection t)

;; This function uses org-mode support for plain list to facilitate dragging
;; URLs from a webbrowser (or other apps) to an org-mode buffer
;; http://www.emacswiki.org/emacs/OrgMode
(defadvice dnd-insert-text (around org-mouse-dnd-insert-text activate)
  (if (eq major-mode 'org-mode)
      (progn
	(cond
	 ;; if this is the end of the line then just insert text here
	 ((eolp)
	  (skip-chars-backward " \t")
	  (kill-region (point) (point-at-eol))
	  (unless (looking-back ":") (insert ":"))
	  (insert " "))

	 ;; if this is the beginning of the line then insert before
	 ((and (looking-at " \\|\t")
	       (save-excursion
		 (skip-chars-backward " \t") (bolp)))
	  (beginning-of-line)
	  (looking-at "[ \t]*")
	  (open-line 1)
	  (indent-to (- (match-end 0) (match-beginning 0)))
	  (insert "+ "))

	 ;; if this is a middle of the line, then insert after
	 (t
	  (end-of-line)
	  (newline t)
	  (indent-relative)
	  (insert "+ ")))
	(insert text)
	(beginning-of-line))
    ad-do-it))

;; include all org files from a directory into the agenda
(setq org-agenda-files (file-expand-wildcards "~/.gtd/*.org"))
