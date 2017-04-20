;; emacs config

;; package config
(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ))

(setq package-list
      '(
        base16-theme
	js2-mode
	json-mode
	magit
        markdown-mode
        ruby-mode
        scss-mode
        yaml-mode
	))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ui prefs
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)

(setq linum-format "%d ")
(global-linum-mode 1)

;; theme
(load-theme 'base16-tomorrow-night t t)
(enable-theme 'base16-tomorrow-night)

;; editing prefs
(setq show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(show-paren-mode t)
(setq kill-whole-line t)
(setq require-final-newline t)

;; no backup files or autosaving
(setq make-backup-files nil)
(setq auto-save-default nil)

;; set theme
(require 'base16-theme)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js-indent-level 2)

;; ruby-mode
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; scss-mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq scss-compile-at-save nil)
(setq css-indent-level 2)
(setq css-indent-offset 2)

;; magit settings
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; other plugins
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-highlight-level 3)
 '(package-selected-packages (quote (yaml-mode json-mode magit js2-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
