;;; Definers

(use-package general
  :config (general-evil-setup))

(general-create-definer define-keys
  :states '(n m v i e))

(general-create-definer define-leader-keys
  :states '(n v i e)
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(general-create-definer define-local-keys
  :major-modes t
  :states '(n v i e)
  :prefix ","
  :non-normal-prefix "M-,")

(general-unbind
  [insert]
  [insertchar]
  [C-down-mouse-1]
  [C-down-mouse-2]
  [C-down-mouse-3]
  "C-x C-z")

(define-keys
  [S-insert] #'mfiano/yank-primary-selection
  [remap move-beginning-of-line] #'mfiano/smarter-move-beginning-of-line
  [remap newline] #'newline-and-indent
  "M--" #'text-scale-decrease
  "M-+" #'text-scale-increase
  "M-=" (fn! (text-scale-set 0)))

(define-leader-keys
  "SPC" '(counsel-M-x :wk t)
  "TAB" '(persp-switch :wk t)
  "'" '(ivy-resume :wk t)
  ";" '(eval-expression :wk t)
  "u" '(universal-argument :wk t))

(define-leader-keys
  :infix "a"
  "" '(:ignore t :wk "app")
  "T" `(,(fn! (term (getenv "SHELL"))) :wk "terminal"))

(define-leader-keys
  :infix "b"
  "" '(:ignore t :wk "buffer")
  "a" '(persp-add-buffer :wk "add")
  "b" '(purpose-x-persp-switch-buffer :wk "switch (same workspace)")
  "B" '(purpose-friendly-switch-buffer :wk "switch")
  "d" '(kill-buffer :wk "delete")
  "D" '(persp-kill :wk "delete")
  "r" '(persp-remove-buffer :wk "remove"))

(define-leader-keys
  :infix "f"
  "" '(:ignore t :wk "file")
  "c" `(,(fn! (call-interactively 'write-file)) :wk "copy")
  "d" '(dired-jump :wk "directory")
  "D" '(mfiano/delete-file :wk "delete")
  "f" '(purpose-friendly-find-file :wk "find")
  "r" '(counsel-recentf :wk "recent")
  "R" '(mfiano/rename-file :wk "rename")
  "s" '(save-buffer :wk "save")
  "S" '(evil-write-all :wk "save all"))

(define-leader-keys
  :infix "g"
  "" '(:ignore t :wk "git")
  "b" '(magit-blame-addition :wk "blame")
  "c" '(forge-create-post :wk "post comment")
  "C" '(forge-edit-post :wk "edit comment")
  "g" '(gist-region-or-buffer :wk "gist")
  "G" '(gist-region-or-buffer-private :wk "gist (private)")
  "i" '(forge-browse-issues :wk "browse issues")
  "n" '(magit-init :wk "initialize")
  "r" '(git-gutter:revert-hunk :wk "revert hunk")
  "R" '(vc-revert :wk "revert file")
  "s" '(magit-status :wk "status")
  "t" '(git-timemachine-toggle :wk "time machine")
  "w" '(browse-at-remote :wk "browse remote"))

(define-leader-keys
  :infix "h"
  "" '(:ignore t :wk "help")
  "." '(helpful-at-point :wk "point")
  "a" '(apropos :wk "apropos")
  "c" '(describe-char :wk "character")
  "f" '(helpful-callable :wk "function")
  "F" '(describe-face :wk "face")
  "i" '(info-lookup-symbol :wk "info")
  "k" '(helpful-key :wk "key")
  "l" '(find-library :wk "library")
  "m" '(describe-minor-mode :wk "minor mode")
  "M" '(describe-mode :wk "major mode")
  "v" '(helpful-variable :wk "variable"))

(define-leader-keys
  :infix "l"
  "" '(:ignore t :wk "layout")
  "l" '(purpose-load-window-layout :wk "load")
  "r" '(purpose-reset-window-layout :wk "reset")
  "s" '(purpose-save-window-layout :wk "save"))

(define-leader-keys
  :infix "o"
  "" '(:ignore t :wk "org")
  "a" '(archive :wk "archive")
  "A" '(org-agenda :wk "agenda")
  "c" '(counsel-org-capture :wk "capture")
  "f" '(org-refile :wk "refile")
  "t" '(org-babel-tangle :wk "tangle"))

(define-leader-keys
  :infix "p"
  "" '(:ignore t :wk "project")
  "C" '(projectile-invalidate-cache :wk "clear cache")
  "f" '(counsel-projectile-find-file :wk "find file")
  "k" '(projectile-kill-buffers :wk "kill")
  "r" '(projectile-recentf :wk "recent project files")
  "p" '(counsel-projectile-switch-project :wk "switch project")
  "R" '(projectile-replace :wk "replace text")
  "s" '(projectile-save-project-buffers :wk "save"))

(define-leader-keys
  :infix "q"
  "" '(:ignore t :wk "quit")
  "q" '(evil-quit-all :wk "quit")
  "Q" '(evil-save-and-quit :wk "save/quit")
  "r" '(restart-emacs :wk "restart"))

(define-leader-keys
  :infix "s"
  "" '(:ignore t :wk "search")
  "b" '(swiper :wk "buffer")
  "d" `(,(fn! (counsel-rg nil default-directory)) :wk "directory")
  "m" '(evil-multiedit-match-all :wk "multi-edit")
  "p" '(counsel-projectile-rg :wk "project"))

(define-leader-keys
  :infix "t"
  "" '(:ignore t :wk "toggle")
  "c" '(flycheck-mode :wk "syntax checker")
  "d" '(diff-hl-mode :wk "diff highlighting")
  "i" '(indent-guide-mode :wk "indent guides")
  "l" '(nlinum-mode :wk "line numbers")
  "s" '(flyspell-mode :wk "spell checker"))

(define-leader-keys
  :infix "v"
  "" '(:ignore t :wk "views")
  "a" '(ivy-push-view :wk "add")
  "r" '(ivy-pop-view :wk "remove")
  "v" '(ivy-switch-view :wk "switch"))

(define-leader-keys
  :infix "w"
  "" '(:ignore t :wk "window")
  "-" '(evil-window-split :wk "split horizontal")
  "|" '(evil-window-vsplit :wk "split vertical")
  "=" '(balance-windows :wk "balance")
  "d" '(evil-window-delete :wk "delete")
  "D" '(delete-other-windows :wk "delete other")
  "f" '(make-frame :wk "new frame")
  "F" '(delete-frame :wk "delete-frame")
  "g" '(mfiano/window-toggle-size :wk "toggle size")
  "l" '(purpose-toggle-window-purpose-dedicated :wk "lock purpose")
  "L" '(purpose-toggle-window-buffer-dedicated :wk "lock buffer")
  "p" '(purpose-set-window-purpose :wk "set purpose")
  "r" '(winner-redo :wk "redo")
  "s" '(ace-swap-window :wk "swap")
  "u" '(winner-undo :wk "undo")
  "w" '(ace-window :wk "go to"))

(define-keys 'special-mode-map
  "q" #'quit-window)
