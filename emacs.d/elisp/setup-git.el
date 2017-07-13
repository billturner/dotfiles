;;; setup-git.el

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(provide 'setup-git)
