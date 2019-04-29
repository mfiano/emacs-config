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
