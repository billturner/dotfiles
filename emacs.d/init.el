;;; init.el

;; package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Load settings
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'setup-path)
(require 'setup-config)
(require 'setup-ui)
(require 'setup-editing)
;; (require 'setup-ivy)
;; (require 'setup-git)
;; (require 'setup-javascript)
(require 'setup-web)
(require 'setup-rjsx)
;; (require 'setup-ruby)
;; (require 'setup-markdown)
(require 'setup-org)
;; (require 'setup-lisp)
(require 'setup-my-functions)
