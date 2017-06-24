;;; init.el

;; package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;
;; basic configurations and customization

;; use y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; use UTF-8
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ui adjustments
(setq inhibit-startup-message t)
(column-number-mode t)
(line-number-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(display-time)
(blink-cursor-mode -1)

;; backups
(setq auto-save-default nil)
(setq make-backup-files nil)

;; always add final newline
(setq require-final-newline t)

;; editing options
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq kill-whole-line t)
(setq show-trailing-whitespace t)
(setq sentence-end-double-space nil)

;; iTerm2 fixes
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])
(define-key input-decode-map "\e[1;3A" [M-up])
(define-key input-decode-map "\e[1;3B" [M-down])
(define-key input-decode-map "\e[1;3C" [M-right])
(define-key input-decode-map "\e[1;3D" [M-left])

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; some global vars
(setq my-org-directory "~/src/org")
(setq my-default-org-file "~/src/org/organizer.org")
(setq my-work-org-file "~/src/org/work.org")


(use-package highlight-chars
  :ensure t
  :config
  ;; (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)
  (add-hook 'font-lock-mode-hook 'hc-highlight-tabs))

;; fill-column-indicator
(use-package fill-column-indicator
  :ensure t
  :init
  (progn
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))
    (setq-default fill-column 80)
    (global-fci-mode 1)))

;; line numbers
;; (use-package nlinum
;;   :ensure t
;;   :config
;;   (setq nlinum-format "%d ")
;;   (global-nlinum-mode))

;; theme
(use-package base16-theme
  :ensure t
  :init (load-theme 'base16-tomorrow-night t))

;; ivy, +ag, +projectile
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-s" . swiper)
         ("C-r" . swiper))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  )
(use-package counsel
  :ensure t
  :after ivy
  :bind ("M-x" . counsel-M-x))
(use-package projectile
  :ensure t
  :diminish (projectile-mode . "pr")
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-files "tags")
  (add-to-list 'projectile-globally-ignored-files "TAGS"))
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :bind (("C-x f" . counsel-projectile-find-file)
         ("C-x a" . counsel-projectile-ag))
  :config
  (counsel-projectile-on))

;; neotree
(use-package neotree
  :ensure t
  :bind (("C-x t" . neotree-toggle))
  :config
  (progn
    (setq neo-window-width 40)
    (setq neo-dont-be-alone t)))

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . "sp")
  :config
  (add-hook 'js-mode-hook #'smartparens-mode)
  (use-package smartparens-config)
  (smartparens-global-mode 1))

;; js2-mode
(use-package js2-mode
  :ensure t
  :defer t
  ;; :diminish (js2-mode)
  :mode (("\\.js$" . js2-mode))
  :commands (js2-mode)
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-basic-offset 2
        js-indent-level 2
        js-switch-indent-offset 2))

;; js2-refactor
(use-package js2-refactor
  :ensure t
  :defer t
  :config
  (js2r-add-keybindings-with-prefix "C-c r")
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

;; flycheck, eslint
(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  ;; (add-hook 'js2-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (setq flycheck-indication-mode 'left-fringe)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))


;; json-mode
(use-package json-mode
  :ensure t
  :defer t
  :diminish (json-mode . "json")
  :mode (("\\.json\\'" . json-mode))
  :commands (json-mode))

;; web-mode
(use-package web-mode
  :ensure t
  :defer t
  :diminish (web-mode . "web")
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (string= major-mode "web-mode")
                (turn-off-fci-mode))))
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq sgml-basic-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-index-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-current-element-highlight t))

;; ruby-mode
(use-package ruby-mode
  :ensure t
  :defer t
  :diminish (ruby-mode . "rb")
  :mode (("\\.rake$" . ruby-mode)
         ("\\.rb$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("Gemfile$" . ruby-mode)))

;; scss-mode
(use-package scss-mode
  :ensure t
  :defer t
  :diminish (scss-mode . "scss")
  :mode (("\\.scss$" . scss-mode))
  :config
  (setq scss-compile-at-save nil
        css-indent-level 2
        css-indent-offset 2))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :defer t
  :diminish (yaml-mode . "yml")
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("README\\.md$" . gfm-mode))
  :commands (markdown-mode gfm-mode))

;; haml-mode
(use-package haml-mode
  :ensure t
  :defer t
  :diminish (haml-mode . "haml"))

;; org-mode
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file my-default-org-file)))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file my-work-org-file)))
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c r" . org-refile)
         ("C-c a" . org-agenda))
  :diminish (org-mode . "org")
  :init (add-hook 'org-mode-hook (lambda () (nlinum-mode -1)))
  :config
  (add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (string= major-mode "org-mode")
              (turn-off-fci-mode))))
  (progn
    (setq org-directory my-org-directory)
    (setq org-return-follows-link t)
    (setq org-log-done 'time)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline my-default-org-file "Inbox")
             "* TODO %^{Task}\n\n")
            ("i" "Idea" entry (file+headline my-default-org-file "Ideas")
             "* %^{Idea} / %t\n\n")
            ("w" "Work Todo" entry (file+headline my-work-org-file "Inbox")
             "* TODO %^{Task}\n\n")
            )
          )
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
    ))

;; lisp, slime
(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t))))

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

(defun my/shift-text (distance)
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

(defun my/shift-right (count)
  (interactive "p")
  (my/shift-text count))

(defun my/shift-left (count)
  (interactive "p")
  (my/shift-text (- count)))

(global-set-key (kbd "C-x >") (lambda () (interactive) (my/shift-right 2)))
(global-set-key (kbd "C-x <") (lambda () (interactive) (my/shift-left 2)))

(defun my/reload-emacs-config ()
  "Reload emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c C-l") 'my/reload-emacs-config)
