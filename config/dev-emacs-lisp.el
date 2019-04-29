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
