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

(use-package lispy
  :config
  (setq lispy-use-sly t
        lispy-colon-p nil)
  (dolist (hook mfiano/lisp-hooks)
    (add-hook hook (fn (lispy-mode 1)))))

(use-package lispyville
  :after lispy
  :config
  (lispyville-set-key-theme
   '(operators
     escape
     (additional-movement normal visual motion)
     text-objects
     atom-movement
     commentary
     slurp/barf-cp))
  (add-hook 'lispy-mode-hook #'lispyville-mode))
