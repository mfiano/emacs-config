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
