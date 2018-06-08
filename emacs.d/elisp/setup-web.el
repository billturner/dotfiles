;;; setup-web.el

;; web-mode
(use-package web-mode
  :ensure t
  :defer t
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
        sgml-attribute-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-index-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-current-element-highlight t))

;; scss-mode
(use-package scss-mode
  :ensure t
  :defer t
  :mode (("\\.scss$" . scss-mode))
  :config
  (setq scss-compile-at-save nil
        css-indent-level 2
        css-indent-offset 2))

(provide 'setup-web)
