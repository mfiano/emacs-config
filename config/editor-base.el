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

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "history" mfiano/dir-etc)
        history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-autosave-interval 120
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  (savehist-mode 1))

(use-package subword
  :init (global-subword-mode 1)
  :diminish subword-mode)

(use-package eldoc
  :commands turn-on-eldoc-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1)
  :diminish eldoc-mode)

(use-package uniquify
  :straight nil
  :config (setq uniquify-buffer-bane-style 'forward))

(use-package recentf
  :commands recentf-mode
  :init
  (setq recentf-save-file (expand-file-name "recent" mfiano/dir-etc)
        recentf-max-saved-items 1000
        recentf-exclude '(".emacs.d/src"
                          ".emacs.d/etc"
                          ".emacs.d/straight"
                          "^/tmp/"
                          "COMMIT_EDITMSG$"
                          ".gz$")
        recentf-auto-cleanup 300
        recentf-filename-handlers '(abbreviate-file-name))
  (recentf-mode 1)
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (add-hook 'kill-emacs-hook #'recentf-save-list))

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-remote-files t
        global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)

(use-package saveplace
  :init (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "places" mfiano/dir-etc)
        save-place-forget-unreadable-files nil))

(use-package ag
  :defer t
  :config
  (setq ag-highlight-search t
        ag-reuse-window nil
        ag-reuse-buffers t))

(use-package url
  :config
  (setq url-cookie-file (expand-file-name "url-cookies" mfiano/dir-etc)))

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

(use-package imenu-anywhere
  :after ivy
  :config
  (setq imenu-anywhere-buffer-filter-functions
        '(imenu-anywhere-same-project-p)))

(use-package fill
  :straight nil
  :commands (turn-on-auto-fill auto-fill-mode)
  :init
  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook 'turn-on-auto-fill))
  (add-hook 'prog-mode-hook
            (fn (setq-local comment-auto-fill-only-comments t)
                (auto-fill-mode 1)))
  (diminish 'auto-fill-function))

(use-package delsel
  :config (delete-selection-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :config
  (setq ivy-count-format "%d/%d ")
  (ivy-mode 1)
  :diminish ivy-mode)

(use-package ivy
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-initial-inputs-alist nil
        ivy-wrap t
        ivy-height 20))

(use-package company
  :init
  (global-company-mode)
  :config
  (company-tng-configure-default)
  (setq company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-idle-delay nil
        completion-at-point-functions '(company-complete-common))
  :diminish company-mode)

(define-keys
  :states 'i
  [tab] #'company-complete-common-or-cycle)

(define-keys 'company-active-map
  [tab] #'company-select-next
  [backtab] #'company-select-previous)

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name
                                     "tramp-history" mfiano/dir-etc)))

(use-package help-mode
  :straight nil
  :config (setq help-window-select t))

(use-package helpful)

(define-keys '(help-mode helpful-mode-map)
  :states 'n
  "q" #'quit-window)

(use-package smex
  :config
  (setq smex-save-file (expand-file-name "smex-items" mfiano/dir-etc)))
