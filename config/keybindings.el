;;; Definers

(use-package general
  :config (general-evil-setup))

(general-create-definer define-keys)

(general-create-definer define-leader-keys
  :states '(n v i e)
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(general-create-definer define-local-keys
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
  "M-=" (fn! (text-scale-set 0))
  "M-1" #'eyebrowse-switch-to-window-config-1
  "M-2" #'eyebrowse-switch-to-window-config-2
  "M-3" #'eyebrowse-switch-to-window-config-3
  "M-4" #'eyebrowse-switch-to-window-config-4
  "M-5" #'eyebrowse-switch-to-window-config-5
  "M-6" #'eyebrowse-switch-to-window-config-6
  "M-7" #'eyebrowse-switch-to-window-config-7
  "M-8" #'eyebrowse-switch-to-window-config-8
  "M-9" #'eyebrowse-switch-to-window-config-9)

(define-leader-keys
  "SPC" '(counsel-M-x :wk t)
  "'" '(ivy-resume :wk t)
  ";" '(eval-expression :wk t)
  "u" '(universal-argument :wk t))

(define-leader-keys
  :infix "a"
  "" '(:ignore t :wk "app")
  "f" '(elfeed :wk "feed reader")
  "T" `(,(fn! (term (getenv "SHELL"))) :wk "terminal"))

(define-leader-keys
  :infix "b"
  "" '(:ignore t :wk "buffer")
  "b" '(switch-to-buffer :wk "switch")
  "d" '(kill-this-buffer :wk "delete"))

(define-leader-keys
  :infix "f"
  "" '(:ignore t :wk "file")
  "c" '(mfiano/copy-file :wk "copy")
  "d" '(dired-jump :wk "directory")
  "D" '(mfiano/delete-file :wk "delete")
  "f" '(counsel-find-file :wk "find")
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
;;
(define-leader-keys
  :infix "p"
  "" '(:ignore t :wk "project")
  "b" '(counsel-projectile-switch-to-buffer :wk "find buffer")
  "c" '(projectile-kill-buffers :wk "close")
  "C" '(projectile-invalidate-cache :wk "clear cache")
  "f" '(counsel-projectile-find-file :wk "find file")
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
  :infix "w"
  "" '(:ignore t :wk "window")
  "-" '(evil-window-split :wk "split horizontal")
  "|" '(evil-window-vsplit :wk "split vertical")
  "=" '(balance-windows "balance")
  "1" '(winum-select-window-by-number :wk "select window")
  "d" '(evil-window-delete :wk "delete")
  "D" '(delete-other-windows :wk "delete other")
  "f" '(make-frame :wk "new frame")
  "F" '(delete-frame :wk "delete-frame")
  "g" '(ace-window :wk "go to")
  "l" '(:ignore t :wk "layouts")
  "l1" '(eyebrowse-switch-to-window-config-1 :wk "layout 1")
  "l2" '(eyebrowse-switch-to-window-config-2 :wk "layout 2")
  "l3" '(eyebrowse-switch-to-window-config-3 :wk "layout 3")
  "l4" '(eyebrowse-switch-to-window-config-4 :wk "layout 4")
  "l5" '(eyebrowse-switch-to-window-config-5 :wk "layout 5")
  "l6" '(eyebrowse-switch-to-window-config-6 :wk "layout 6")
  "l7" '(eyebrowse-switch-to-window-config-7 :wk "layout 7")
  "l8" '(eyebrowse-switch-to-window-config-8 :wk "layout 8")
  "l9" '(eyebrowse-switch-to-window-config-9 :wk "layout 9")
  "r" '(winner-redo :wk "redo")
  "s" '(eyebrowse-switch-to-window-config :wk "switch layout")
  "S" '(ace-swap-window :wk "swap")
  "u" '(winner-undo :wk "undo"))

(define-keys 'special-mode-map
  "q" #'quit-window)

;; (general-define-key
;;  :states '(normal visual)
;;  :prefix "<SPC>"
;;  "ft" 'neotree-toggle
;;  "jc" 'avy-goto-char
;;  "jl" 'avy-goto-line
;;  "jw" 'avy-goto-word-1
;;  "k" 'lisp-state-toggle-lisp-state
;;  "oa" 'org-agenda
;;  "oA" 'archive
;;  "oc" 'counsel-org-capture
;;  "of" 'org-refile
;;  "ot" 'org-babel-tangle
