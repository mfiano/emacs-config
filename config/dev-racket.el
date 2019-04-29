(use-package racket-mode
  :commands racket-unicode-input-method-enable
  :init
  (setq tab-always-indent t)
  (dolist (hook '(racket-mode-hook racket-repl-mode-hook))
    (add-hook hook 'racket-unicode-input-method-enable)))
