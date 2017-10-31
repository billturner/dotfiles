;;; setup-rjsx.el

;; (use-package add-node-modules-path
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'web-mode-hook #'add-node-modules-path)
;;     (add-hook 'js2-mode-hook #'add-node-modules-path)
;;     (with-eval-after-load 'rjsx-mode
;;       (add-hook 'rjsx-mode-hook #'add-node-modules-path))))

(use-package js2-mode
  :ensure t
  :init
  (setq
   js-basic-indent 2
   js-basic-offset 2
   js-switch-indent-offset 2)
  (setq-default
   js-switch-indent-offset 2
   js2-basic-indent 2
   js2-basic-offset 2
   js2-auto-indent-p t
   js2-cleanup-whitespace t
   js2-enter-indents-newline t
   js2-indent-on-enter-key t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode)))

(provide 'setup-rjsx)
