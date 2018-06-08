;;; setup-projectile.el

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x f" . helm-projectile-find-file)
         ("C-x C-b" . helm-mini)
         ("C-x a" . helm-projectile-ag)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-files "tags")
  (add-to-list 'projectile-globally-ignored-files "TAGS"))

(use-package helm-projectile
  :ensure t)
(helm-projectile-on)

(use-package projectile-rails
  :ensure t)
(projectile-rails-global-mode)

(use-package helm-ag
  :ensure t)

(provide 'setup-projectile)
