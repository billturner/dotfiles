;;; setup-ivy.el

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
  :diminish (projectile-mode . "Proj")
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

(provide 'setup-ivy)
