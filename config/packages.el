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
  (run-at-time nil 120 (lambda () (quiet! (recentf-save-list))))
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
        ivy-use-selectable-prompt t
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

(use-package dired
  :straight nil
  :config
  (define-keys 'dired-mode-map
    "q" #'quit-window))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  :config
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.35)
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
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (set-face-background 'diff-hl-change (doom-color 'orange))
  (set-face-background 'diff-hl-delete (doom-color 'red))
  (set-face-background 'diff-hl-insert (doom-color 'green)))

(use-package indent-guide
  :commands indent-guide-mode
  :config (set-face-foreground 'indent-guide-face "dimgray")
  :diminish indent-guide-mode)

(use-package visual-fill-column
  :config (setq visual-fill-column-width nil))

(use-package winner
  :config (winner-mode 1))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

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
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
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

(use-package aggressive-indent
  :config
  (add-hook 'prog-mode-hook (fn (aggressive-indent-mode 1)))
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  :diminish aggressive-indent-mode)

(use-package evil-commentary)

(define-keys
  :states '(n v)
  "gc" #'evil-commentary)

(use-package hungry-delete
  :after smartparens
  :config
  (global-hungry-delete-mode)
  (add-hook 'smartparens-enabled-hook (fn (hungry-delete-mode 1)))
  :diminish hungry-delete-mode)

(use-package undo-tree
  :commands (undo-tree-save-history-hook undo-tree-load-history-hook)
  :init
  (let ((undo-dir (file-name-as-directory (expand-file-name "undo" mfiano/dir-etc))))
    (make-directory undo-dir t)
    (add-hook 'write-file-functions 'undo-tree-save-history-hook)
    (add-hook 'find-file-hook 'undo-tree-load-history-hook)
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist `(("." . ,undo-dir))
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t))
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode 1)
  :diminish whitespace-cleanup-mode)

(use-package expand-region
  :defer t)

(define-keys
  :states 'v
  "v" #'er/expand-region
  "V" #'er/contract-region)

(use-package evil-multiedit)

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package avy
  :commands (avy-goto-char avy-goto-word-1 avy-goto-line)
  :config
  (setq avy-style 'pre
        avy-all-windows nil
        avy-background t
        avy-keys (nconc (number-sequence ?a ?z)
                        (number-sequence ?A ?Z)
                        (number-sequence ?1 ?9))))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package perspective
  :config
  (persp-mode))

(use-package window-purpose
  :config
  (purpose-mode)
  (setq purpose-x-popwin-position 'right
        purpose-x-popwin-width 0.4
        purpose-user-mode-purposes '((lisp-mode . lisp)
                                     (sly-inspector-mode . debug)
                                     (sly-xref-mode . debug)
                                     (sly-popup-buffer-mode . debug))
        purpose-user-regexp-purposes '(("\\*sly-db.\**" . debug)
                                       ("\\*sly-mrepl.\**" . repl))
        purpose-user-name-purposes '(("*sly-compilation*" . debug)
                                     ("*sly-macroexpansion*" . debug)
                                     ("*sly-description*" . debug)))
  (purpose-compile-user-configuration)
  (purpose-x-magit-single-on)
  (purpose-x-persp-setup)
  (purpose-x-popwin-setup))

(use-package gist
  :defer t
  :config (setq gist-view-gist t)
  :diminish gist-mode)

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

(use-package executable
  :commands executable-make-buffer-file-executable-if-script-p
  :init (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package rainbow-mode
  :commands 'rainbow-mode
  :init
  (dolist (hook '(web-mode-hook css-mode-hook))
    (add-hook hook 'rainbow-mode))
  :diminish rainbow-mode)

(use-package web-mode
  :defer t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-enable-auto-pairing t))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :init (add-hook 'css-mode-hook 'turn-on-css-eldoc))

(defvar mfiano/lisp-hooks
  '(ielm-mode-hook
    lisp-interaction-mode-hook
    eval-expression-minibuffer-setup-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    sly-mrepl-mode-hook
    clojure-mode-hook
    cider-repl-mode-hook
    scheme-mode-hook))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (dolist (hook mfiano/lisp-hooks)
    (add-hook hook 'rainbow-delimiters-mode))
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'show-paren-mismatch))

(use-package paren
  :commands show-paren-mode
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis))

(use-package elec-pair
  :config (electric-pair-mode 1))

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  :diminish evil-smartparens-mode)

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (dolist (hook mfiano/lisp-hooks)
    (add-hook hook 'smartparens-strict-mode))
  :diminish smartparens-mode)

(use-package evil-cleverparens
  :init
  (setq evil-move-beyond-eol t
        evil-cleverparens-swap-move-by-word-and-symbol t
        evil-cleverparens-use-regular-insert t)
  (dolist (hook mfiano/lisp-hooks)
    (add-hook hook 'evil-cleverparens-mode))
  :diminish evil-cleverparens-mode)

(use-package macrostep
  :after evil-cleverparens
  :config
  (define-keys 'n 'macrostep-keymap
    [tab] #'macrostep-next-macro
    [backtab] #'macrostep-prev-macro
    "c" #'macrostep-collapse
    "e" #'macrostep-expand
    "q" #'macrostep-collapse-all)
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps))

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  :diminish elisp-slime-nav-mode)

(use-package ielm
  :defer t
  :bind
  (:map ielm-map
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)))

(define-local-keys emacs-lisp-mode-map
  "'" '(ielm :wk "repl"))

(define-local-keys emacs-lisp-mode-map
  :infix "g"
  "" '(:ignore t :wk "go")
  "b" '(pop-tag-mark :wk "pop definition")
  "d" '(elisp-slime-nav-find-elisp-thing-at-point :wk "find definition"))

(defvar mfiano/lisp-implementations
  '((sbcl ("ros" "-L" "sbcl" "run"))
    (qlot ("qlot" "exec" "ros" "-L" "sbcl" "run"))
    (sbcl-nvidia ("primusrun" "ros" "-L" "sbcl-bin" "run"))
    (sbcl-git ("ros" "-L" "sbcl/git" "run"))
    (ccl ("ros" "-L" "ccl-bin" "run"))))

(defvar mfiano/clhs-path (file-name-as-directory
                          (expand-file-name "~/.data/common-lisp/clhs")))

(use-package sly
  :defer t
  :commands sly
  :bind
  (:map sly-mrepl-mode-map
        ("<up>" . sly-mrepl-previous-input-or-button)
        ("<down>" . sly-mrepl-next-input-or-button))
  :init
  (evil-set-initial-state 'sly-mrepl-mode 'insert)
  (evil-set-initial-state 'sly-inspector-mode 'emacs)
  (evil-set-initial-state 'sly-db-mode 'emacs)
  (evil-set-initial-state 'sly-xref-mode 'emacs)
  (evil-set-initial-state 'sly-stickers--replay-mode 'emacs)
  :config
  (setq sly-lisp-implementations mfiano/lisp-implementations
        sly-mrepl-history-file-name (expand-file-name "sly-repl-history"
                                                      mfiano/dir-etc)
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol*-fancy t
        common-lisp-hyperspec-root mfiano/clhs-path)
  (sly-setup '(sly-fancy))
  (add-hook 'sly-mode-hook #'evil-normalize-keymaps)
  (add-hook 'sly-popup-buffer-mode-hook #'evil-normalize-keymaps)
  :diminish sly)

(use-package sly-macrostep
  :after sly)

(use-package sly-repl-ansi-color
  :after sly
  :config (push 'sly-repl-ansi-color sly-contribs))

(define-local-keys (lisp-mode-map sly-mrepl-mode-map)
  "'" '(sly :wk "start")
  ";" `(,(fn! (let ((current-prefix-arg '-)) (sly nil nil t)))
        :wk "start (ask)")
  "," '(mfiano/cycle-sly-repls :wk "focus repl"))

(define-local-keys lisp-mode-map
  :infix "c"
  "" '(:ignore t :wk "compile")
  "c" '(sly-compile-file :wk "compile file")
  "C" '(sly-compile-and-load-file :wk "compile/load file")
  "f" '(sly-compile-defun :wk "compile top-level form")
  "l" '(sly-load-file :wk "load file")
  "n" '(sly-remove-notes :wk "remove notes")
  "r" '(sly-compile-region :wk "compile region"))

(define-local-keys lisp-mode-map
  :infix "e"
  "" '(:ignore t :wk "evaluate")
  "b" '(sly-eval-buffer :wk "buffer")
  "e" '(sly-eval-last-expression :wk "last expression")
  "f" '(sly-eval-defun :wk "function")
  "F" '(sly-undefine-function :wk "undefine function")
  "r" '(sly-eval-region :wk "region"))

(define-local-keys lisp-mode-map
  :infix "g"
  "" '(:ignore t :wk "go")
  "b" '(sly-pop-find-definition-stack :wk "back")
  "d" '(sly-edit-definition :wk "definition")
  "D" '(sly-edit-definition-other-window :wk "definition (other window)")
  "n" '(sly-next-note :wk "next note")
  "N" '(sly-previous-note :wk "previous note")
  "s" '(sly-stickers-next-sticker :wk "next sticker")
  "S" '(sly-stickers-prev-sticker :wk "previous sticker"))

(define-local-keys lisp-mode-map
  :infix "h"
  "" '(:ignore t :wk "help")
  "<" '(sly-who-calls :wk "who calls")
  ">" '(sly-calls-who :wk "calls who")
  "~" '(hyperspec-lookup-format :wk "lookup format directive")
  "#" '(hyperspec-lookup-reader-macro :wk "lookup reader macro")
  "a" '(sly-apropos :wk "apropos")
  "b" '(sly-who-binds :wk "who binds")
  "d" '(sly-disassemble-symbol :wk "disassemble symbol")
  "h" '(sly-describe-symbol :wk "describe symbol")
  "H" '(sly-hyperspec-lookup :wk "hyperspec lookup")
  "m" '(sly-who-macroexpands :wk "who macro-expands")
  "p" '(sly-apropos-package :wk "apropos package")
  "r" '(sly-who-references :wk "who references")
  "s" '(sly-who-specializes :wk "who specializes")
  "S" '(sly-who-sets :wk "who sets"))

(define-local-keys (emacs-lisp-mode-map sly-mrepl-mode-map lisp-mode-map)
  :infix "l"
  "" '(:ignore t :wk "lisp")
  "a" '(sp-absorb-sexp :wk "absorb")
  "b" '(sp-forward-barf-sexp :wk "barf forward")
  "B" '(sp-backward-barf-sexp :wk "barf backward")
  "c" '(sp-convolute-sexp :wk "convolute")
  "e" '(sp-splice-sexp-killing-forward :wk "splice killing forward")
  "E" '(sp-splice-sexp-killing-backward :wk "splice killing backward")
  "j" '(sp-join-sexp :wk "join")
  "r" '(sp-raise-sexp :wk "raise")
  "s" '(sp-forward-slurp-sexp :wk "slurp forward")
  "S" '(sp-backward-slurp-sexp :wk "slurp backward")
  "t" '(sp-transpose-sexp :wk "transpose")
  "w" '(sp-wrap-round :wk "wrap")
  "W" '(sp-unwrap-sexp :wk "unwrap"))

(define-local-keys lisp-mode-map
  "m" '(macrostep-expand :wk "macro expand"))

(define-local-keys lisp-mode-map
  :infix "r"
  "" '(:ignore t :wk "repl")
  "c" '(sly-mrepl-clear-repl :wk "clear")
  "q" '(sly-quit-lisp :wk "quit")
  "r" '(sly-restart-inferior-lisp :wk "restart")
  "s" '(sly-mrepl-sync :wk "sync"))

(define-local-keys lisp-mode-map
  :infix "s"
  "" '(:ignore t :wk "stickers")
  "b" '(sly-stickers-toggle-break-on-stickers :wk "toggle break")
  "c" '(sly-stickers-clear-defun-stickers :wk "clear function")
  "C" '(sly-stickers-clear-buffer-stickers :wk "clear buffer")
  "f" '(sly-stickers-fetch :wk "fetch")
  "r" '(sly-stickers-replay :wk "replay")
  "s" '(sly-stickers-dwim :wk "add/remove"))

(define-local-keys lisp-mode-map
  :infix "t"
  "" '(:ignore t :wk "trace")
  "t" '(sly-toggle-trace-fdefinition :wk "toggle")
  "T" '(sly-toggle-fancy-trace :wk "toggle (fancy)")
  "u" '(sly-untrace-all :wk "untrace all"))

(evil-set-initial-state 'sly-db-mode 'normal)
(evil-set-initial-state 'sly-inspector-mode 'normal)
(evil-set-initial-state 'sly-popup-buffer-mode 'normal)
(evil-set-initial-state 'sly-xref-mode 'normal)

(define-keys i sly-mrepl-mode-map
  [S-return] #'newline-and-indent
  [up] (fn! (evil-goto-line) (comint-previous-input 1))
  [down] (fn! (evil-goto-line) (comint-next-input 1)))

(define-keys n sly-popup-buffer-mode-map
  "q" #'quit-window)

(define-keys n sly-db-mode-map
  [follow-link] #'mouse-face
  [remap quit-window] #'sly-db-quit
  "C-i" #'sly-db-cycle
  "0" #'sly-db-invoke-restart-0
  "1" #'sly-db-invoke-restart-1
  "2" #'sly-db-invoke-restart-2
  "3" #'sly-db-invoke-restart-3
  "4" #'sly-db-invoke-restart-4
  "5" #'sly-db-invoke-restart-5
  "6" #'sly-db-invoke-restart-6
  "7" #'sly-db-invoke-restart-7
  "8" #'sly-db-invoke-restart-8
  "9" #'sly-db-invoke-restart-9
  "a" #'sly-db-abort
  "A" #'sly-db-break-with-system-debugger
  "b" #'sly-db-break-on-return
  "B" #'sly-db-break-with-default-debugger
  "c" #'sly-db-continue
  "C" #'sly-db-inspect-condition
  "d" #'sly-db-pprint-eval-in-frame
  "D" #'sly-db-disassemble
  "e" #'sly-db-eval-in-frame
  "gg" #'sly-db-beginning-of-backtrace
  "gr" #'sly-db-restart-frame
  "G" #'sly-db-end-of-backtrace
  "i" #'sly-db-inspect-in-frame
  "I" #'sly-db-invoke-restart-by-name
  "n" #'sly-db-next
  "o" #'sly-db-out
  "P" #'sly-db-print-condition
  "q" #'sly-db-quit
  "R" #'sly-db-return-from-frame
  "s" #'sly-db-step
  "S" #'sly-db-show-frame-source
  "t" #'sly-db-toggle-details)

(define-keys n sly-inspector-mode-map
  [backtab] #'backward-button
  [return] #'push-button
  [(shift tab)] #'backward-button
  [M-return] #'sly-mrepl-copy-part-to-repl
  "e" #'sly-inspector-eval
  "gb" #'sly-inspector-pop
  "gr" #'sly-inspector-reinspect
  "gR" #'sly-inspector-fetch-all
  "gv" #'sly-inspector-toggle-verbose
  "h" #'sly-inspector-history
  "i" #'sly-inspector-describe-inspectee
  "p" #'sly-button-pretty-print
  "q" #'sly-inspector-quit)

(define-keys n sly-xref-mode-map
  [return] #'sly-goto-xref
  [S-return] #'sly-show-xref
  "q" #'quit-window)

(use-package clojure-mode)

(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("<up>" . cider-repl-previous-input)
        ("<down>" . cider-repl-next-input))
  :config
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'cider-inspector-mode 'emacs)
  (setq nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-clojure-cli-global-options "-A:dev"
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-repl-print-length 20
        cider-repl-history-display-duplicates nil
        cider-repl-history-size 1000
        cider-repl-history-file (expand-file-name "cider-repl-history"
                                                  mfiano/dir-etc))
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  "'" '(cider-jack-in :wk "start"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "e"
  "" '(:ignore t :wk "evaluate")
  "e" '(cider-eval-last-sexp :wk "last expression")
  "E" '(cider-insert-last-sexp-in-repl :wk "insert last expression")
  "f" '(cider-eval-defun-at-point :wk "function")
  "F" '(cider-insert-defun-in-repl :wk "insert function")
  "r" '(cider-eval-region :wk "region")
  "R" '(cider-insert-region-in-repl :wk "insert region")
  "u" '(cider-undef :wk "undefine"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "g"
  "" '(:ignore t :wk "go")
  "b" '(cider-pop-back :wk "pop")
  "v" '(cider-find-var :wk "find variable")
  "n" '(cider-find-ns :wk "find namespace"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "h"
  "" '(:ignore t :wk "help")
  "a" '(cider-apropos :wk "apropos")
  "d" '(cider-doc :wk "open documentation")
  "g" '(cider-grimoire-web :wk "grimoire")
  "j" '(cider-javadoc :wk "javadoc"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "i"
  "" '(:ignore t :wk "inspect")
  "i" '(cider-inspect :wk "inspect")
  "r" '(cider-inspect-last-result :wk "last result"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "m"
  "" '(:ignore t :wk "macro")
  "e" '(cider-macroexpand-1 :wk "macro-expand")
  "E" '(cider-macroexpand-all :wk "macro-expand all"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "n"
  "" '(:ignore t :wk "namespace")
  "n" '(cider-browse-ns :wk "browse")
  "N" '(cider-browse-ns-all :wk "browse all"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "r"
  "" '(:ignore t :wk "repl")
  "n" '(cider-repl-set-ns :wk "set namespace")
  "q" '(cider-quit :wk "quit")
  "r" '(cider-restart :wk "restart")
  "R" '(cider-refresh :wk "refresh"))

(use-package racket-mode
  :commands racket-unicode-input-method-enable
  :init
  (setq tab-always-indent t)
  (dolist (hook '(racket-mode-hook racket-repl-mode-hook))
    (add-hook hook 'racket-unicode-input-method-enable)))

(use-package lua-mode
  :config
  (setq lua-indent-level 2))
