;;; setup-editing.el

;; use UTF-8
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; editing options
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq kill-whole-line t)
(setq show-trailing-whitespace t)
(setq sentence-end-double-space nil)
(delete-selection-mode 1)

;; always add final newline
(setq require-final-newline t)

;; backups
(setq auto-save-default nil)
(setq make-backup-files nil)

(provide 'setup-editing)
