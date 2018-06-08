;;; setup-ruby.el

;; ruby-mode
(use-package ruby-mode
  :ensure t
  :defer t
  :mode (("\\.rake$" . ruby-mode)
         ("\\.rb$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("Gemfile$" . ruby-mode)))

(use-package ruby-test-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-test-mode))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;; haml-mode
(use-package haml-mode
  :ensure t
  :defer t)

(provide 'setup-ruby)
