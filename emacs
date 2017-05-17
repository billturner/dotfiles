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
        ;; expand-region
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
        smartparens
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
(fset 'yes-or-no-p 'y-or-n-p)

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

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; no backup files or autosaving
(setq make-backup-files nil)
(setq auto-save-default nil)

;; iterm2 fixes for M-S
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])

(define-key input-decode-map "\e[1;3A" [M-up])
(define-key input-decode-map "\e[1;3B" [M-down])
(define-key input-decode-map "\e[1;3C" [M-right])
(define-key input-decode-map "\e[1;3D" [M-left])

;; fill-column-indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)

;; highlight chars
(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; orgmode
(setq org-return-follows-link t)
(add-hook 'org-mode-hook (lambda () (nlinum-mode -1)))

;; org-capture
(setq org-directory "~/src/org")
(setq org-default-personal-file "~/src/org/organizer.org")
(setq org-default-work-file "~/src/org/work.org")
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file org-default-personal-file)))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file org-default-work-file)))
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "orangered" :weight bold))
        ("NEXT" . (:foreground "steelblue1" :weight bold))
        ("WAITING" . (:foreground "lightgoldenrod" :weight bold))
        ("DONE" . (:foreground "forestgreen" :weight bold))
        ("CANCELLED" . (:foreground "forestgreen" :weight bold))
        ))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-personal-file "Inbox")
         "* TODO %^{Task}\n\n")
        ("i" "Idea" entry (file+headline org-default-personal-file "Ideas")
         "* %^{Idea} / %t\n\n")
        ("w" "Work Todo" entry (file+headline org-default-work-file "Inbox")
         "* TODO %^{Task}\n\n")
        )
      )
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

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
(add-to-list 'projectile-globally-ignored-directories ".git")
(add-to-list 'projectile-globally-ignored-directories "build")
(add-to-list 'projectile-globally-ignored-directories "dist")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "vendor")
(add-to-list 'projectile-globally-ignored-files ".DS_Store")
(add-to-list 'projectile-globally-ignored-files "tags")
(add-to-list 'projectile-globally-ignored-files "TAGS")
(helm-mode 1)

;; js2-mode
(require 'js2-mode)
(add-hook 'js-mode-hook #'js2-minor-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

;; smartparens
(require 'smartparens-config)
;; only in js2-mode for now
(add-hook 'js-mode-hook #'smartparens-mode)

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

;; expand-region
;; (require 'expand-region)
;; (global-set-key (kbd "C-@") 'er/expand-region)
;; ;; temporary fix for https://github.com/magnars/expand-region.el/issues/220
;; (setq shift-select-mode nil)

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

;; personal functions
;; commenting function from: http://stackoverflow.com/a/9697222/17773
(defun my/comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-logical-line)))
(global-set-key (kbd "M-;") 'my/comment-or-uncomment-region-or-line)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

(global-set-key (kbd "C-x >") (lambda () (interactive) (shift-right 2)))
(global-set-key (kbd "C-x <") (lambda () (interactive) (shift-left 2)))

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
