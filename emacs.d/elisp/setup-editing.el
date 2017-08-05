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

;; always add final newline
(setq require-final-newline t)

(use-package highlight-chars
  :ensure t
  :config
  ;; (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)
  (add-hook 'font-lock-mode-hook 'hc-highlight-tabs))

;; fill-column-indicator
;; (use-package fill-column-indicator
;;   :ensure t
;;   :init
;;   (progn
;;     (define-globalized-minor-mode global-fci-mode fci-mode
;;       (lambda () (fci-mode 1)))
;;     (setq-default fill-column 80)
;;     (global-fci-mode 1)))

;; line numbers
;; (use-package nlinum
;;   :ensure t
;;   :config
;;   (setq nlinum-format "%d ")
;;   (global-nlinum-mode))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . "Sp")
  :config
  (add-hook 'js-mode-hook #'smartparens-mode)
  (use-package smartparens-config)
  (smartparens-global-mode 1))
  (sp-local-pair 'js-mode "{" nil :post-handlers '((my/newline-and-enter-parens "RET")))
  (sp-local-pair 'js2-mode "{" nil :post-handlers '((my/newline-and-enter-parens "RET")))

;; backups
(setq auto-save-default nil)
(setq make-backup-files nil)

(provide 'setup-editing)
