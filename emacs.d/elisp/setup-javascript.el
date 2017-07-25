;;; setup-javascript.el

;; js2-mode
(use-package js2-mode
  :ensure t
  :defer t
  ;; :diminish (js2-mode)
  :mode (("\\.jsx?$" . js2-mode))
  :commands (js2-mode)
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-basic-offset 2
        js-indent-level 2
        js-switch-indent-offset 2))

;; js2-refactor
(use-package js2-refactor
  :ensure t
  :defer t
  :config
  (js2r-add-keybindings-with-prefix "C-c r")
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

;; prettier
(use-package prettier-js
  :ensure t
  :defer t
  :bind ("C-c p" . "prettier-js")
  :init
  (defun my/use-prettier-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (prettier (and root
                        (expand-file-name "node_modules/.bin/prettier"
                                          root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-js-command prettier))))
  ;; (add-hook 'js2-mode-hook 'prettier-js-mode)
  ;; (add-hook 'js2-mode-hook #'my/use-prettier-from-node-modules)
  ;; (add-hook 'web-mode-hook 'prettier-js-mode)
  ;; (add-hook 'web-mode-hook #'my/use-prettier-from-node-modules)
  (remove-hook 'before-save-hook 'prettier-js)
  ;; remove-hook doesn't seem to work, so need to manually enable mode
  :config
  (setq prettier-js-args '(
                           "--single-quote" "true"
                           "--bracket-spacing" "true"
                           "--trailing-comma" "none"
                           ))
  )
(with-eval-after-load 'prettier-js
  (remove-hook 'before-save-hook 'prettier-js t)
  (remove-hook 'before-save-hook #'prettier-js t))

;; flycheck, eslint
(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  ;; (add-hook 'js2-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (setq flycheck-indication-mode 'left-fringe)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))


;; json-mode
(use-package json-mode
  :ensure t
  :defer t
  :diminish (json-mode . "json")
  :mode (("\\.json\\'" . json-mode))
  :commands (json-mode))

(provide 'setup-javascript)
