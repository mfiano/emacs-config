(use-package org
  :defer t
  :config
  (setq org-directory mfiano/dir-org
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-catch-invisible-edits 'show-and-error
        org-publish-timestamp-directory (expand-file-name "org-timestamps" mfiano/dir-etc)
        org-html-todo-kwd-class-prefix "keyword "
        org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-startup-indented t
        org-ellipsis " […]"
        org-return-follows-link t
        org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-src-preserve-indentation t
        org-startup-folded t
        org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
                            (sequence "REPORTED(r@/!)" "BUG(b@/!)" "|" "FIXED(f@/!)"))
        org-todo-keyword-faces '(("TODO" :foreground "dodger blue" :weight bold)
                                 ("INPROGRESS" :foreground "spring green" :weight bold)
                                 ("WAITING" :foreground "yellow" :weight bold)
                                 ("HOLD" :foreground "yellow" :weight bold)
                                 ("DONE" :foreground "forest green" :weight bold)
                                 ("CANCELLED" :foreground "forest green" :weight bold)
                                 ("REPORTED" :foreground "red" :weight bold)
                                 ("BUG" :foreground "red" :weight bold)
                                 ("FIXED" :foreground "forest green" :weight bold))
        org-capture-templates '(("c" "Code Task" entry (file+headline org-default-notes-file
                                                                      "Coding Tasks")
                                 "* TODO %?\n  Entered on: %U - %a\n")
                                ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
                                 "* TODO %?\n  Entered on: %U")
                                ("n" "Note" entry (file+olp+datetree org-default-notes-file)
                                 "* %?\n\n"))))

(use-package org-indent
  :straight nil
  :after org
  :defer t
  :diminish org-indent-mode)

(use-package org-bullets
  :config
  (setq-default org-bullets-bullet-list '("✸"))
  (add-hook 'org-mode-hook (fn (org-bullets-mode 1))))
