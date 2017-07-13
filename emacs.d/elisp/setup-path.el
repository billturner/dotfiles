;;; setup-path.el

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init
  (progn
    (when (memq window-system '(mac ns x))
      ; (add-to-list 'exec-path-from-shell-variables "GOPATH")
      (add-hook 'eshell-mode-hook 'exec-path-from-shell-initialize)
      (exec-path-from-shell-initialize))))

(provide 'setup-path)
