;;; setup-ui.el

;; ui adjustments
(setq inhibit-startup-message t)
(column-number-mode t)
(line-number-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(display-time)
(blink-cursor-mode -1)

;; use y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; setup initial position and size
(when window-system
  (set-frame-position (selected-frame) 50 75)
  (set-frame-size (selected-frame) 130 45))

;; setup font
;; Hack font => http://sourcefoundry.org/hack/
(when window-system
  (set-face-attribute 'default nil :font "Hack" :height 120))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

;; neotree
(use-package neotree
  :ensure t
  :bind (("C-x t" . neotree-toggle))
  :config
  (progn
    (setq neo-show-hidden-files t)
    (setq neo-window-width 40)
    (setq neo-dont-be-alone t)))

(provide 'setup-ui)
