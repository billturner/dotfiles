;;; setup-markdown.el

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("README\\.md$" . gfm-mode))
  :commands (markdown-mode gfm-mode)
  :config (progn
            (add-hook 'markdown-mode-hook #'visual-line-mode))
  )

(provide 'setup-markdown)