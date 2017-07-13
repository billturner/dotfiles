;;; setup-ruby.el

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

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :defer t
  :diminish (yaml-mode . "yml")
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;; haml-mode
(use-package haml-mode
  :ensure t
  :defer t
  :diminish (haml-mode . "haml"))

(provide 'setup-ruby)
