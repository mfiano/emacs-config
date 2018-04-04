(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-unset-key (kbd "<S-down-mouse-1>"))
(global-unset-key (kbd "<S-down-mouse-3>"))
(global-unset-key (kbd "<C-down-mouse-1>"))
(global-unset-key (kbd "<C-down-mouse-3>"))
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "<C-insert>"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "<S-insert>") 'my/yank-primary-selection)
(global-set-key [remap move-beginning-of-line] 'my/smarter-move-beginning-of-line)

(which-key-add-key-based-replacements
  "<f13>b" "buffers"
  "<f13>c" "comments"
  "<f13>f" "files"
  "<f13>F" "frames"
  "<f13>g" "git"
  "<f13>h" "help"
  "<f13>j" "jumps"
  "<f13>l" "layouts"
  "<f13>lb" "buffers"
  "<f13>o" "org-mode"
  "<f13>p" "projects"
  "<f13>q" "quit"
  "<f13>s" "search"
  "<f13>w" "windows")

(general-define-key
 :prefix "<f13>"
 "<f13>" 'counsel-M-x
 "<tab>" 'next-buffer
 "<backtab>" 'previous-buffer
 "1" 'winum-select-window-1
 "2" 'winum-select-window-2
 "3" 'winum-select-window-3
 "4" 'winum-select-window-4
 "5" 'winum-select-window-5
 "6" 'winum-select-window-6
 "7" 'winum-select-window-7
 "8" 'winum-select-window-8
 "9" 'winum-select-window-9
 "qq" 'kill-emacs
 "ba" 'persp-add-buffer
 "bb" 'counsel-ibuffer
 "bd" 'kill-this-buffer
 "bf" 'focus-mode
 "bn" 'nlinum-mode
 "br" 'persp-remove-buffer
 "cl" 'evilnc-comment-or-uncomment-lines
 "cp" 'evilnc-comment-or-uncomment-paragraphs
 "fC" 'my/copy-file
 "fD" 'my/delete-file
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf
 "fR" 'my/rename-file
 "fs" 'save-buffer
 "fS" 'evil-write-all
 "ft" 'neotree-toggle
 "Fd" 'delete-frame
 "Fn" 'make-frame
 "F+" 'frame-font-bigger
 "F-" 'frame-font-smaller
 "gg" 'gist-region-or-buffer
 "gG" 'gist-region-or-buffer-private
 "gs" 'magit-status
 "hc" 'describe-char
 "hf" 'describe-function
 "hk" 'describe-key
 "hm" 'describe-mode
 "hv" 'describe-variable
 "jc" 'avy-goto-char
 "jl" 'avy-goto-line
 "jw" 'avy-goto-word-1
 "k" 'lisp-state-toggle-lisp-state
 "lba" 'persp-add-buffer
 "lbb" 'persp-switch-to-buffer
 "lbd" 'persp-kill-buffer
 "lbr" 'persp-remove-buffer
 "ll" 'persp-load-state-from-file
 "lr" 'persp-rename
 "ls" 'persp-save-state-to-file
 "oa" 'org-agenda
 "oA" 'archive
 "oc" 'counsel-org-capture
 "of" 'org-refile
 "ot" 'org-babel-tangle
 "pb" 'counsel-projectile-switch-to-buffer
 "pf" 'counsel-projectile-find-file
 "pI" 'projectile-invalidate-cache
 "pk" 'projectile-kill-buffers
 "pp" 'counsel-projectile-switch-project
 "pr" 'counsel-projectile-recentf
 "ps" 'counsel-projectile-rg
 "pS" 'projectile-save-project-buffers
 "P" 'paradox-list-packages
 "sp" 'counsel-projectile-rg
 "ss" 'swiper
 "sS" 'highlight-symbol
 "w|" 'split-window-right
 "w-" 'split-window-below
 "w=" 'balance-windows
 "w1" 'eyebrowse-switch-to-window-config-1
 "w2" 'eyebrowse-switch-to-window-config-2
 "w3" 'eyebrowse-switch-to-window-config-3
 "w4" 'eyebrowse-switch-to-window-config-4
 "w5" 'eyebrowse-switch-to-window-config-5
 "w6" 'eyebrowse-switch-to-window-config-6
 "w7" 'eyebrowse-switch-to-window-config-7
 "w8" 'eyebrowse-switch-to-window-config-8
 "w9" 'eyebrowse-switch-to-window-config-9
 "w <tab>" 'eyebrowse-last-window-config
 "w <left>" 'winner-undo
 "w <right>" 'winner-redo
 "wd" 'delete-window
 "wm" 'delete-other-windows
 "ww" 'ace-window)
