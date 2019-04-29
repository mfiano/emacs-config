(defvar mfiano/lisp-implementations
  '((sbcl ("ros" "-L" "sbcl" "run"))
    (qlot ("qlot" "exec" "ros" "-L" "sbcl" "run"))
    (sbcl-nvidia ("primusrun" "ros" "-L" "sbcl-bin" "run"))
    (sbcl-git ("ros" "-L" "sbcl/git" "run"))
    (ccl ("ros" "-L" "ccl-bin" "run"))))

(defvar mfiano/clhs-path (file-name-as-directory
                          (expand-file-name "~/.data/common-lisp/clhs")))

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
  (setq sly-lisp-implementations mfiano/lisp-implementations
        sly-mrepl-history-file-name (expand-file-name "sly-repl-history"
                                                      mfiano/dir-etc)
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol*-fancy t
        common-lisp-hyperspec-root mfiano/clhs-path)
  (sly-setup '(sly-fancy))
  (add-hook 'sly-mode-hook #'evil-normalize-keymaps)
  (add-hook 'sly-popup-buffer-mode-hook #'evil-normalize-keymaps)
  :diminish sly)

(use-package sly-macrostep
  :after sly)

(use-package sly-repl-ansi-color
  :after sly
  :config (push 'sly-repl-ansi-color sly-contribs))

(define-local-keys lisp-mode-map
  "'" '(sly :wk "start")
  ";" `(,(fn! (let ((current-prefix-arg '-)) (sly nil nil t)))
        :wk "start (ask)"))

(define-local-keys lisp-mode-map
  :infix "c"
  "" '(:ignore t :wk "compile")
  "c" '(sly-compile-file :wk "compile file")
  "C" '(sly-compile-and-load-file :wk "compile/load file")
  "f" '(sly-compile-defun :wk "compile top-level form")
  "l" '(sly-load-file :wk "load file")
  "n" '(sly-remove-notes :wk "remove notes")
  "r" '(sly-compile-region :wk "compile region"))

(define-local-keys lisp-mode-map
  :infix "e"
  "" '(:ignore t :wk "evaluate")
  "b" '(sly-eval-buffer :wk "buffer")
  "e" '(sly-eval-last-expression :wk "last expression")
  "f" '(sly-eval-defun :wk "function")
  "F" '(sly-undefine-function :wk "undefine function")
  "r" '(sly-eval-region :wk "region"))

(define-local-keys lisp-mode-map
  :infix "g"
  "" '(:ignore t :wk "go")
  "b" '(sly-pop-find-definition-stack :wk "back")
  "d" '(sly-edit-definition :wk "definition")
  "D" '(sly-edit-definition-other-window :wk "definition (other window)")
  "n" '(sly-next-note :wk "next note")
  "N" '(sly-previous-note :wk "previous note")
  "s" '(sly-stickers-next-sticker :wk "next sticker")
  "S" '(sly-stickers-prev-sticker :wk "previous sticker"))

(define-local-keys lisp-mode-map
  :infix "h"
  "" '(:ignore t :wk "help")
  "<" '(sly-who-calls :wk "who calls")
  ">" '(sly-calls-who :wk "calls who")
  "~" '(hyperspec-lookup-format :wk "lookup format directive")
  "#" '(hyperspec-lookup-reader-macro :wk "lookup reader macro")
  "a" '(sly-apropos :wk "apropos")
  "b" '(sly-who-binds :wk "who binds")
  "d" '(sly-disassemble-symbol :wk "disassemble symbol")
  "h" '(sly-describe-symbol :wk "describe symbol")
  "H" '(sly-hyperspec-lookup :wk "hyperspec lookup")
  "m" '(sly-who-macroexpands :wk "who macro-expands")
  "p" '(sly-apropos-package :wk "apropos package")
  "r" '(sly-who-references :wk "who references")
  "s" '(sly-who-specializes :wk "who specializes")
  "S" '(sly-who-sets :wk "who sets"))

(define-local-keys lisp-mode-map
  "m" '(macrostep-expand :wk "macro expand"))

(define-local-keys lisp-mode-map
  :infix "r"
  "" '(:ignore t :wk "repl")
  "c" '(sly-mrepl-clear-repl :wk "clear")
  "q" '(sly-quit-lisp :wk "quit")
  "r" '(sly-restart-inferior-lisp :wk "restart")
  "s" '(sly-mrepl-sync :wk "sync"))

(define-local-keys lisp-mode-map
  :infix "s"
  "" '(:ignore t :wk "stickers")
  "b" '(sly-stickers-toggle-break-on-stickers :wk "toggle break")
  "c" '(sly-stickers-clear-defun-stickers :wk "clear function")
  "C" '(sly-stickers-clear-buffer-stickers :wk "clear buffer")
  "f" '(sly-stickers-fetch :wk "fetch")
  "r" '(sly-stickers-replay :wk "replay")
  "s" '(sly-stickers-dwim :wk "add/remove"))

(define-local-keys lisp-mode-map
  :infix "t"
  "" '(:ignore t :wk "trace")
  "t" '(sly-toggle-trace-fdefinition :wk "toggle")
  "T" '(sly-toggle-fancy-trace :wk "toggle (fancy)")
  "u" '(sly-untrace-all :wk "untrace all"))

(evil-set-initial-state 'sly-db-mode 'normal)
(evil-set-initial-state 'sly-inspector-mode 'normal)
(evil-set-initial-state 'sly-popup-buffer-mode 'normal)
(evil-set-initial-state 'sly-xref-mode 'normal)

(define-keys i sly-mrepl-mode-map
  [S-return] #'newline-and-indent
  [backspace] #'sp-backward-delete-char
  [up] (fn! (evil-goto-line) (comint-previous-input 1))
  [down] (fn! (evil-goto-line) (comint-next-input 1)))

(define-keys n sly-popup-buffer-mode-map
  "q" #'quit-window)

(define-keys n sly-db-mode-map
  [follow-link] #'mouse-face
  [remap quit-window] #'sly-db-quit
  "C-i" #'sly-db-cycle
  "0" #'sly-db-invoke-restart-0
  "1" #'sly-db-invoke-restart-1
  "2" #'sly-db-invoke-restart-2
  "3" #'sly-db-invoke-restart-3
  "4" #'sly-db-invoke-restart-4
  "5" #'sly-db-invoke-restart-5
  "6" #'sly-db-invoke-restart-6
  "7" #'sly-db-invoke-restart-7
  "8" #'sly-db-invoke-restart-8
  "9" #'sly-db-invoke-restart-9
  "a" #'sly-db-abort
  "A" #'sly-db-break-with-system-debugger
  "b" #'sly-db-break-on-return
  "B" #'sly-db-break-with-default-debugger
  "c" #'sly-db-continue
  "C" #'sly-db-inspect-condition
  "d" #'sly-db-pprint-eval-in-frame
  "D" #'sly-db-disassemble
  "e" #'sly-db-eval-in-frame
  "gg" #'sly-db-beginning-of-backtrace
  "gr" #'sly-db-restart-frame
  "G" #'sly-db-end-of-backtrace
  "i" #'sly-db-inspect-in-frame
  "I" #'sly-db-invoke-restart-by-name
  "n" #'sly-db-next
  "o" #'sly-db-out
  "P" #'sly-db-print-condition
  "q" #'sly-db-quit
  "R" #'sly-db-return-from-frame
  "s" #'sly-db-step
  "S" #'sly-db-show-frame-source
  "t" #'sly-db-toggle-details)

(define-keys n sly-inspector-mode-map
  [backtab] #'backward-button
  [return] #'push-button
  [(shift tab)] #'backward-button
  [M-return] #'sly-mrepl-copy-part-to-repl
  "e" #'sly-inspector-eval
  "gb" #'sly-inspector-pop
  "gr" #'sly-inspector-reinspect
  "gR" #'sly-inspector-fetch-all
  "gv" #'sly-inspector-toggle-verbose
  "h" #'sly-inspector-history
  "i" #'sly-inspector-describe-inspectee
  "p" #'sly-button-pretty-print
  "q" #'sly-inspector-quit)

(define-keys n sly-xref-mode-map
  [return] #'sly-goto-xref
  [S-return] #'sly-show-xref
  "q" #'quit-window)
