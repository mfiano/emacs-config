(use-package nlinum
  :config
  (global-nlinum-mode 1)
  (setq nlinum-format "%4d "))

(use-package nlinum-hl
  :config
  (add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
  (advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
  (advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  :config
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.3)
  (dimmer-mode))

(use-package all-the-icons-ivy
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump
                            counsel-recentf
                            counsel-projectile-find-file
                            counsel-projectile-find-dir))
  (all-the-icons-ivy-setup))

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5
        which-key-sort-order 'which-key-key-order-alpha
        which-key-key-replacement-alist '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                                          ("left" . "◀")
                                          ("right" . "▶")
                                          ("up" . "▲")
                                          ("down" . "▼")
                                          ("delete" . "DEL")
                                          ("\\`DEL\\'" . "BKSP")
                                          ("next" . "PgDn")
                                          ("prior" . "PgUp")))
  :diminish which-key-mode)

(use-package diff-hl
  :commands (diff-hl-dired-mode diff-hl-flydiff-mode)
  :init
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package indent-guide
  :commands indent-guide-mode
  :config (set-face-foreground 'indent-guide-face "dimgray")
  :diminish indent-guide-mode)

(use-package visual-fill-column
  :config (setq visual-fill-column-width nil))
