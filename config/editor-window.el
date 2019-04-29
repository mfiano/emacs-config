(use-package winner
  :config (winner-mode 1))

(use-package winum
  :init (setq winum-auto-setup-mode-line nil)
  :config (winum-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(use-package persp-mode
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-save-dir (file-name-as-directory (expand-file-name "persp-conf" mfiano/dir-etc))
        persp-auto-resume-time 1
        persp-auto-save-num-of-backups 0
        persp-auto-save-opt 0
        persp-set-last-persp-for-new-frames nil)
  (add-hook 'after-init-hook (fn (persp-mode 1)))
  :diminish persp-mode)

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

(use-package window-purpose
  :config (purpose-x-magit-single-on))

(use-package ivy-purpose
  :config (ivy-purpose-setup))
