;;; setup-org.el

;; org vars
(setq my-org-directory "~/src/org")
(setq my-default-org-file "~/src/org/organizer.org")
(setq my-work-org-file "~/src/org/work.org")

;; org-mode
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file my-default-org-file)))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file my-work-org-file)))
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c r" . org-refile)
         ("C-c a" . org-agenda))
  :diminish (org-mode . "org")
  :init (add-hook 'org-mode-hook (lambda () (nlinum-mode -1)))
  :config
  (add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (string= major-mode "org-mode")
              (turn-off-fci-mode))))
  (progn
    (setq org-directory my-org-directory)
    (setq org-return-follows-link t)
    (setq org-log-done 'time)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline my-default-org-file "Inbox")
             "* TODO %^{Task}\n\n")
            ("i" "Idea" entry (file+headline my-default-org-file "Ideas")
             "* %^{Idea} / %t\n\n")
            ("w" "Work Todo" entry (file+headline my-work-org-file "Inbox")
             "* TODO %^{Task}\n\n")
            )
          )
    (setq org-todo-keywords
          '(
            (sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-todo-keyword-faces
          '(("TODO" . (:foreground "orangered" :weight bold))
            ("NEXT" . (:foreground "steelblue1" :weight bold))
            ("WAITING" . (:foreground "lightgoldenrod" :weight bold))
            ("DONE" . (:foreground "forestgreen" :weight bold))
            ("CANCELLED" . (:foreground "forestgreen" :weight bold))
            ))
    ))

(provide 'setup-org)
