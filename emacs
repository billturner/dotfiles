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
        add-node-modules-path
        base16-theme
        fill-column-indicator
        helm
        helm-ag
        helm-projectile
        highlight-chars
        js2-mode
        json-mode
        magit
        markdown-mode
        neotree
        nlinum
        ;; paredit
        projectile
        ruby-mode
        scss-mode
        slime
        web-mode
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

;; line numbers
(setq nlinum-format "%d ")
(global-nlinum-mode)

;; set up theme
(require 'base16-theme)
(load-theme 'base16-tomorrow-night t t)
(enable-theme 'base16-tomorrow-night)

;; editing prefs
(setq show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(show-paren-mode t)
(setq kill-whole-line t)
(setq require-final-newline t)

;; modeline changes
(setq display-time-24hr-format t)
(display-time-mode +1)

;; no backup files or autosaving
(setq make-backup-files nil)
(setq auto-save-default nil)

;; fill-column-indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)

;; highlight chars
(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; org mode
(setq org-return-follows-link t)
(add-hook 'org-mode-hook (lambda () (nlinum-mode -1)))


;; neotree setup
(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)
(setq neo-window-width 40)
;; (setq neo-smart-open t)

;; helm & projectile & ag
(require 'helm)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(require 'helm-config)

(require 'helm-projectile)
(projectile-global-mode)
(helm-projectile-on)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x a") 'helm-projectile-ag)
(setq projectile-globally-ignored-directories
      (quote
       (".git", "build", "dist", "node_modules", "vendor")))
(setq projectile-globally-ignored-files
      (quote
       (".DS_Store", "TAGS", "tags")))
(helm-mode 1)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)

;; web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

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

;; lisp, quicklisp, and slime
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; (require 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fill-column-indicator yaml-mode json-mode magit js2-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
