(require 'package)

(setq package-user-dir (concat user-emacs-directory "src")
      package-enable-at-startup nil
      package--init-file-ensured t
      use-package-always-ensure t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package diminish)

(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package executable
  :commands executable-make-buffer-file-executable-if-script-p
  :init (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "history" my/dir-etc)
        history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-autosave-interval 120
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

(use-package undo-tree
  :commands (undo-tree-save-history-hook undo-tree-load-history-hook)
  :init
  (let ((undo-dir (file-name-as-directory (expand-file-name "undo" my/dir-etc))))
    (make-directory undo-dir t)
    (add-hook 'write-file-functions 'undo-tree-save-history-hook)
    (add-hook 'find-file-hook 'undo-tree-load-history-hook)
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist `(("." . ,undo-dir))
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t))
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package avy
  :commands (avy-goto-char avy-goto-word-1 avy-goto-line)
  :config
  (setq avy-style 'pre
        avy-all-windows nil
        avy-background t
        avy-keys (nconc (number-sequence ?a ?z)
                        (number-sequence ?A ?Z)
                        (number-sequence ?1 ?9))))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode 1)
  :diminish whitespace-cleanup-mode)

(use-package delsel
  :config (delete-selection-mode 1))

(use-package subword
  :init (global-subword-mode 1)
  :diminish subword-mode)

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

(use-package nlinum
  :config
  (global-nlinum-mode 1)
  (setq nlinum-format "%4d "))

(use-package hydra
  :defer t)

(use-package paradox
  :commands paradox-list-packages
  :config
  (setq paradox-execute-asynchronously t
        paradox-display-download-count t
        paradox-display-star-count t
        paradox-github-token t))

(use-package eldoc
  :commands turn-on-eldoc-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1)
  :diminish eldoc-mode)

(use-package url
  :ensure nil
  :config (setq url-cookie-file (expand-file-name "url-cookies" my/dir-etc)))

(use-package goto-addr
  :commands (goto-address-mode goto-address-prog-mode)
  :init
  (add-hook 'text-mode-hook 'goto-address-mode)
  (add-hook 'prog-mode-hook 'goto-address-prog-mode))

(use-package browse-url-dwim
  :commands browse-url-xdg-open
  :init
  (setq browse-url-browser-function 'browse-url-xdg-open
        browse-url-dwim-always-confirm-extraction nil))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  :config
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(use-package spaceline
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (spaceline-helm-mode)
    (setq powerline-default-separator 'arrow
          spaceline-workspace-numbers-unicode t
          spaceline-window-numbers-unicode t)))

(use-package indent-guide
  :commands indent-guide-mode
  :config (set-face-foreground 'indent-guide-face "dimgray")
  :diminish indent-guide-mode)

(use-package visual-fill-column
  :config (setq visual-fill-column-width nil))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package ag
  :defer t
  :config
  (setq ag-highlight-search t
        ag-reuse-window nil
        ag-reuse-buffers t))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-bane-style 'forward))

(use-package recentf
  :commands recentf-mode
  :init
  (setq recentf-save-file (expand-file-name "recent" my/dir-etc)
        recentf-max-saved-items 1000
        recentf-exclude '(".emacs.d/src"
                          ".emacs.d/etc"
                          "^/tmp/"
                          "COMMIT_EDITMSG$"
                          ".gz$")
        recentf-auto-cleanup 300
        recentf-filename-handlers '(abbreviate-file-name))
  (recentf-mode 1))

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-remote-files t
        global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)

(use-package saveplace
  :init (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "places" my/dir-etc)
        save-place-forget-unreadable-files nil))

(use-package fill
  :ensure nil
  :commands (turn-on-auto-fill auto-fill-mode)
  :init
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook 'turn-on-auto-fill))
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (auto-fill-mode 1)))
  (diminish 'auto-fill-function))

(use-package expand-region
  :defer t)

(use-package projectile
  :init
  (setq projectile-cache-file (expand-file-name "project-cache" my/dir-etc)
        projectile-known-projects-file (expand-file-name "project-bookmarks" my/dir-etc))
  (projectile-global-mode 1)
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  :config
  (setq projectile-find-dir-includes-top-level t
        projectile-globally-ignored-file-suffixes my/ignored-files)
  :diminish projectile-mode)

(use-package helm
  :demand t
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq helm-display-header-line nil
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-split-window-in-side-p t
        helm-M-x-fuzzy-match t
        helm-M-x-requires-pattern nil
        helm-buffers-fuzzy-matching t
        helm-bookmark-show-location t
        helm-recentf-fuzzy-match t
        helm-move-to-line-cycle-in-source nil
        helm-ff-skip-boring-files t
        helm-ff-file-name-history-use-recentf nil
        helm-ff-file-compressed-list '("gz" "bz2" "zip" "tgz" "7z" "xz")
        helm-candidate-number-limit 1000)
  :diminish helm-mode)

(use-package swiper-helm
  :bind (("C-s" . swiper-helm)))

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

(use-package helm-ls-git
  :defer t)

(use-package helm-ag
  :defer t
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-base-command "rg --vimgrep --smart-case --no-heading"))

(use-package magit
  :defer t
  :config
  (setq magit-log-arguments '("--graph" "--decorate" "--color")
        magit-save-repository-buffers 'dontask
        magit-revert-buffers 'silent))

(use-package gist
  :defer t
  :config (setq gist-view-gist t)
  :diminish gist-mode)

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp-history" my/dir-etc)))

(use-package ace-link)

(use-package org
  :defer t
  :config
  (setq org-directory my/dir-org
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-catch-invisible-edits 'show-and-error
        org-publish-timestamp-directory (expand-file-name "org-timestamps" my/dir-etc)
        org-html-todo-kwd-class-prefix "keyword "
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
        org-capture-templates '(("t" "Task" entry (file org-default-notes-file)
                                 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))))

(use-package org-indent
  :ensure nil
  :after org
  :defer t
  :diminish org-indent-mode)

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package fill-column-indicator
  :config
  (setq fci-rule-color "#444"
        fci-rule-use-dashes t
        fci-dash-pattern 0.5))

(use-package flycheck)

(use-package company
  :bind
  (:map prog-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (company-tng-configure-default)
  (setq company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-idle-delay nil
        completion-at-point-functions '(company-complete-common))
  :diminish company-mode)

(use-package evil
  :config
  (setq evil-move-beyond-eol t)
  (evil-mode 1))

(use-package general
  :config (general-evil-setup))

(use-package neotree
  :config
  (evil-set-initial-state 'neotree-mode 'emacs)
  (setq neo-smart-open t
        neo-window-width 40))

(use-package sh-script
  :commands sh-mode
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile$" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("\\.*zshrc$" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  :diminish aggressive-indent-mode)

(use-package evil-nerd-commenter)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(use-package winner
  :config (winner-mode 1))

(use-package persp-mode
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-save-dir (file-name-as-directory (expand-file-name "persp-conf" my/dir-etc))
        persp-auto-resume-time 1
        persp-auto-save-num-of-backups 0
        persp-auto-save-opt 0
        persp-set-last-persp-for-new-frames nil)
  (add-hook 'after-init-hook (lambda () (persp-mode 1)))
  :diminish persp-mode)

(use-package winum
  :init (setq winum-auto-setup-mode-line nil)
  :config (winum-mode))

(use-package eyebrowse
  :config (eyebrowse-mode t))

(use-package hungry-delete
  :config (global-hungry-delete-mode)
  :diminish hungry-delete-mode)

(use-package framesize)

(use-package highlight-symbol)

(use-package smex
  :config (setq smex-save-file (expand-file-name "smex-items" my/dir-etc)))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.3)
  (dimmer-mode))

(use-package popwin
  :commands popwin-mode
  :init (popwin-mode 1)
  :config
  (setq popwin:special-display-config nil)
  (push '("*Help*" :width 0.5 :position right)
        popwin:special-display-config)
  (push '(magit-status-mode :width 0.5 :position right :stick t)
        popwin:special-display-config)
  (push '(neotree-mode :position left :stick t)
        popwin:special-display-config)
  (push '(apropos-mode :width 0.5 :position right)
        popwin:special-display-config))
