(use-package projectile
  :init
  (setq projectile-cache-file (expand-file-name "project-cache" mfiano/dir-etc)
        projectile-known-projects-file (expand-file-name "project-bookmarks" mfiano/dir-etc)
        projectile-kill-buffers-filter 'kill-only-files
        projectile-completion-system 'ivy)
  (projectile-global-mode 1)
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  :config
  (setq projectile-find-dir-includes-top-level t
        projectile-globally-ignored-file-suffixes mfiano/ignored-files)
  :diminish projectile-mode)

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :defer t
  :config
  (magit-wip-mode)
  (setq magit-log-arguments '("--graph" "--decorate" "--color")
        magit-save-repository-buffers 'dontask
        magit-revert-buffers 'silent
        magit-delete-by-moving-to-trash nil
        git-commit-summary-max-length 120))

(use-package evil-magit
  :after magit)

(use-package forge
  :after magit)

(use-package git-timemachine)

(define-keys
  :definer 'minor-mode
  :states 'normal
  :keymaps 'git-timemachine-mode
  "[" #'git-timemachine-show-previous-revision
  "]" #'git-timemachine-show-next-revision
  "b" #'git-timemachine-blame
  "q" #'git-timemachine-quit)

(use-package browse-at-remote)
