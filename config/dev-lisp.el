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

(defvar mfiano/lisp-modes
  '(ielm
    lisp-interaction-mode-map
    emacs-lisp-mode-map
    lisp-mode-map
    sly-mrepl-mode-map
    clojure-mode-map))

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

(use-package macrostep
  :config
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps))

(define-keys 'n 'macrostep-keymap
  [tab] #'macrostep-next-macro
  [backtab] #'macrostep-prev-macro
  "c" #'macrostep-collapse
  "e" #'macrostep-expand
  "q" #'macrostep-collapse-all)

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
        evil-cleverparens-use-additional-movement-keys nil
        evil-cleverparens-use-additional-bindings nil)
  (dolist (hook mfiano/lisp-hooks)
    (add-hook hook 'evil-cleverparens-mode))
  :diminish evil-cleverparens-mode)

(use-package evil-lisp-state
  :init (setq evil-lisp-state-global t))

(use-package macrostep
  :config
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps))

(define-keys 'n 'macrostep-keymap
  [tab] #'macrostep-next-macro
  [backtab] #'macrostep-prev-macro
  "c" #'macrostep-collapse
  "e" #'macrostep-expand
  "q" #'macrostep-collapse-all)
