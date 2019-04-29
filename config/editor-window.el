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

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

(use-package window-purpose
  :config (purpose-x-magit-single-on))

(use-package ivy-purpose
  :config (ivy-purpose-setup))

(use-package shackle
  :config
  (setq shackle-default-size 0.5)
  (shackle-mode 1))
