;;; setup-my-functions.el

;; personal functions
;; commenting function from: http://stackoverflow.com/a/9697222/17773
(defun my/comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-logical-line)))
(global-set-key (kbd "M-;") 'my/comment-or-uncomment-region-or-line)

(defun my/shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun my/shift-right (count)
  (interactive "p")
  (my/shift-text count))

(defun my/shift-left (count)
  (interactive "p")
  (my/shift-text (- count)))

(global-set-key (kbd "C-x >") (lambda () (interactive) (my/shift-right 2)))
(global-set-key (kbd "C-x <") (lambda () (interactive) (my/shift-left 2)))

;; quick-copy-line
;; from https://www.emacswiki.org/emacs/CopyingWholeLines#toc7
(defun my/quick-copy-line ()
  "copy current line and jump to next line"
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))
(global-set-key (kbd "C-c M-d") 'my/quick-copy-line)

;; for javascript
(defun my/newline-and-enter-parens (&rest _ignored)
  "Insert newline with certain parentheses"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/reload-emacs-config ()
  "Reload emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c C-l") 'my/reload-emacs-config)

(provide 'setup-my-functions)
