;;; setup-lisp.el

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

(provide 'setup-lisp)
