(defvar my/lisp-hooks
  '(ielm-mode-hook
    lisp-interaction-mode-hook
    eval-expression-minibuffer-setup-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    sly-mrepl-mode-hook
    clojure-mode-hook
    cider-repl-mode
    scheme-mode-hook))

(defvar my/lisp-modes
  '(ielm
    lisp-interaction-mode-map
    emacs-lisp-mode-map
    lisp-mode-map
    sly-mrepl-mode-map
    clojure-mode-map))

(defvar my/lisp-implementations
  '((sbcl-src/roswell ("ros" "-L" "sbcl" "run"))
    (sbcl-bin/roswell ("ros" "-L" "sbcl-bin" "run"))
    (sbcl-git/roswell ("ros" "-L" "sbcl/git" "run"))
    (ccl-bin/roswell ("ros" "-L" "ccl-bin" "run"))))

(defvar my/clhs-path (file-name-as-directory (expand-file-name "~/.data/common-lisp/clhs")))

(use-package paren
  :commands show-paren-mode
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#32cd32")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis))

(use-package elec-pair
  :config (electric-pair-mode 1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (dolist (hook my/lisp-hooks)
    (add-hook hook 'rainbow-delimiters-mode))
  :config
  (cl-loop with colors = '("#ff4b4b" "#5fafd7")
           for index from 1 to rainbow-delimiters-max-face-count
           do (set-face-foreground
               (intern (format "rainbow-delimiters-depth-%d-face" index))
               (elt colors (if (cl-evenp index) 0 1))))
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'show-paren-mismatch))

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (dolist (hook my/lisp-hooks)
    (add-hook hook 'smartparens-strict-mode))
  :diminish smartparens-mode)

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  :diminish evil-smartparens-mode)

(use-package evil-cleverparens
  :config
  (setq evil-move-beyond-eol t
        evil-cleverparens-use-additional-movement-keys nil
        evil-cleverparens-use-additional-bindings nil)
  (dolist (hook my/lisp-hooks)
    (add-hook hook 'evil-cleverparens-mode))
  :diminish evil-cleverparens-mode)

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-lisp-state
  :init (setq evil-lisp-state-global t))

(use-package ielm
  :ensure nil
  :defer t
  :bind
  (:map ielm-map
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)))

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  :diminish elisp-slime-nav-mode)

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
  (setq sly-lisp-implementations my/lisp-implementations
        sly-mrepl-history-file-name (expand-file-name "sly-repl-history" my/dir-etc)
        sly-autodoc-use-multiline t
        sly-complete-symbol*-fancy t
        sly-kill-without-query-p t
        sly-repl-history-remove-duplicates t
        sly-repl-history-trim-whitespaces t
        sly-net-coding-system 'utf-8-unix
        common-lisp-hyperspec-root my/clhs-path)
  (sly-setup '(sly-fancy))
  (push '("*sly-description*" :width 0.5 :position right)
        popwin:special-display-config)
  (push '("*sly-macroexpansion*" :width 0.5 :position right)
        popwin:special-display-config))

(use-package sly-company
  :config
  (setq sly-company-completion 'fuzzy)
  (add-hook 'sly-mode-hook 'sly-company-mode)
  (add-hook 'sly-mrepl-hook 'sly-company-mode)
  (add-to-list 'company-backends 'sly-company))

(use-package sly-macrostep
  :defer t)

(use-package sly-repl-ansi-color
  :demand t
  :config (push 'sly-repl-ansi-color sly-contribs))

(use-package racket-mode
  :commands racket-unicode-input-method-enable
  :init
  (setq tab-always-indent t)
  (dolist (hook '(racket-mode-hook racket-repl-mode-hook))
    (add-hook hook 'racket-unicode-input-method-enable)))

(use-package clojure-mode)

(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("<up>" . cider-repl-previous-input)
        ("down>" . cider-repl-next-input))
  :config
  (setq nrepl-log-messages nil
        cider-font-lock-dynamically nil
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-size 5000
        cider-repl-history-file (expand-file-name "cider-repl-history" my/dir-etc)))

(general-nmap
  :prefix ","
  :keymaps 'emacs-lisp-mode-map
  "'" 'ielm)

(defhydra my/hydra-sly (:exit nil :hint nil :foreign-keys run)
  "
Common Lisp Sly Navigation

^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_g_] Jump to definition                [_n_] Next compiler note     [_s_] Next sticker
[_G_] Jump to definition (other window) [_N_] Previous compiler note [_S_] Previous sticker
[_b_] Pop from definition

[_q_] Exit
"
  ("g" sly-edit-definition)
  ("G" sly-edit-definition-other-window)
  ("b" sly-pop-find-definition-stack)
  ("n" sly-next-note)
  ("N" sly-previous-note)
  ("s" sly-stickers-next-sticker)
  ("S" sly-stickers-prev-sticker)
  ("q" nil :exit t))

(general-nmap
  :prefix ","
  :keymaps 'sly-mode-map
  "'" 'sly
  "ha" 'sly-apropos
  "hb" 'sly-who-binds
  "hd" 'sly-disassemble-symbol
  "hh" 'sly-describe-symbol
  "hH" 'sly-hyperspec-lookup
  "hm" 'sly-who-macroexpands
  "hp" 'sly-apropos-package
  "hr" 'sly-who-references
  "hs" 'sly-who-specializes
  "hS" 'sly-who-sets
  "h<" 'sly-who-calls
  "h>" 'sly-calls-who
  "cc" 'sly-compile-file
  "cC" 'sly-compile-and-load-file
  "cf" 'sly-compile-defun
  "cl" 'sly-load-file
  "cn" 'sly-remove-notes
  "cr" 'sly-compile-region
  "eb" 'sly-eval-buffer
  "ee" 'sly-eval-last-expression
  "eE" 'sly-eval-print-last-expression
  "ef" 'sly-eval-defun
  "eF" 'slime-undefine-function
  "er" 'sly-eval-region
  "g" 'my/hydra-sly/body
  "me" 'sly-macroexpand-1
  "mE" 'sly-macroexpand-all
  "sc" 'sly-mrepl-clear-repl
  "si" 'sly
  "sq" 'sly-quit-lisp
  "sn" 'sly-mrepl-new
  "sr" 'sly-restart-inferior-lisp
  "ss" 'sly-mrepl-sync
  "Sb" 'sly-stickers-toggle-break-on-stickers
  "Sc" 'sly-stickers-clear-defun-stickers
  "SC" 'sly-stickers-clear-buffer-stickers
  "Sf" 'sly-stickers-fetch
  "Sr" 'sly-stickers-replay
  "Ss" 'sly-stickers-dwim
  "tt" 'sly-toggle-trace-fdefinition
  "tT" 'sly-toggle-fancy-trace
  "tu" 'sly-untrace-all)

(general-nmap
  :prefix ","
  :keymaps 'clojure-mode-map
  "'" 'cider-jack-in
  "gg" 'cider-find-var
  "gb" 'cider-pop-back
  "gn" 'cider-find-ns)
