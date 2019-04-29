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

(use-package gist
  :defer t
  :config (setq gist-view-gist t)
  :diminish gist-mode)
