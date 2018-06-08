;;; setup-org.el

;; org vars
(setq my-org-directory "~/src/org")
(setq my-default-org-file "~/src/org/organizer.org")
(setq my-choice-point-org-file "~/src/org/choice-points.org")
(setq my-work-org-file "~/src/org/work.org")
(setq my-work-devlog-org-file "~/src/org/devlog-work.org")

;; org-mode
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file my-default-org-file)))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file my-work-org-file)))
(global-set-key (kbd "C-c t")
                (lambda () (interactive) (find-file my-choice-point-org-file)))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c r" . org-refile)
         ("C-c a" . org-agenda))
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (progn
    (setq org-directory my-org-directory)
    (setq org-return-follows-link t)
    (setq org-log-done 'time)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline my-default-org-file "Inbox")
             "* TODO %^{Task}\n")
            ("i" "Idea" entry (file+headline my-default-org-file "Ideas")
             "* %^{Idea} / %t\n")
             ("w" "Work Todo" entry (file+headline my-work-org-file "Inbox")
             "* TODO %^{Task}\n")
            ("l" "Work Devlog" entry (file+datetree my-work-devlog-org-file)
             "* %^{Summary}\n\n%?")
            ("c" "Choice Point Action" entry (file+datetree my-choice-point-org-file)
             "* %^{Action} %^g\n")
            )
          )
    (setq org-todo-keywords
          '(
            (sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
            ))
    (setq org-todo-keyword-faces
          '(("TODO" . (:foreground "orangered" :weight bold))
            ("NEXT" . (:foreground "steelblue1" :weight bold))
            ("WAITING" . (:foreground "lightgoldenrod" :weight bold))
            ("DONE" . (:foreground "forestgreen" :weight bold))
            ("CANCELLED" . (:foreground "forestgreen" :weight bold))
            ))
    (setq org-tag-alist
          '(
            (:startgroup . nil)
            ("towards" . ?t)
            ("away" . ?a)
            (:endgroup . nil)
            (:startgroup . nil)
            ("@personal" . ?p)
            ("@work" . ?w)
            ("@project" . ?j)
            ("@blog" . ?b)
            (:endgroup . nil)
            )
          )
    (setq org-tag-faces
          '(
            ("towards" . (:foreground "forestgreen" :weight bold))
            ("away" . (:foreground "orangered" :weight bold))
            ("@personal" . (:foreground "forestgreen" :weight bold))
            )
          )
    ))

(provide 'setup-org)
