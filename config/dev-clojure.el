(use-package clojure-mode)

(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("<up>" . cider-repl-previous-input)
        ("<down>" . cider-repl-next-input))
  :config
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'cider-inspector-mode 'emacs)
  (setq nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-clojure-cli-global-options "-A:dev"
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-repl-print-length 20
        cider-repl-history-display-duplicates nil
        cider-repl-history-size 1000
        cider-repl-history-file (expand-file-name "cider-repl-history"
                                                  mfiano/dir-etc))
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  "'" '(cider-jack-in :wk "start"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "e"
  "" '(:ignore t :wk "evaluate")
  "e" '(cider-eval-last-sexp :wk "last expression")
  "E" '(cider-insert-last-sexp-in-repl :wk "insert last expression")
  "f" '(cider-eval-defun-at-point :wk "function")
  "F" '(cider-insert-defun-in-repl :wk "insert function")
  "r" '(cider-eval-region :wk "region")
  "R" '(cider-insert-region-in-repl :wk "insert region")
  "u" '(cider-undef :wk "undefine"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "g"
  "" '(:ignore t :wk "go")
  "b" '(cider-pop-back :wk "pop")
  "v" '(cider-find-var :wk "find variable")
  "n" '(cider-find-ns :wk "find namespace"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "h"
  "" '(:ignore t :wk "help")
  "a" '(cider-apropos :wk "apropos")
  "d" '(cider-doc :wk "open documentation")
  "g" '(cider-grimoire-web :wk "grimoire")
  "j" '(cider-javadoc :wk "javadoc"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "i"
  "" '(:ignore t :wk "inspect")
  "i" '(cider-inspect :wk "inspect")
  "r" '(cider-inspect-last-result :wk "last result"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "m"
  "" '(:ignore t :wk "macro")
  "e" '(cider-macroexpand-1 :wk "macro-expand")
  "E" '(cider-macroexpand-all :wk "macro-expand all"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "n"
  "" '(:ignore t :wk "namespace")
  "n" '(cider-browse-ns :wk "browse")
  "N" '(cider-browse-ns-all :wk "browse all"))

(define-local-keys (clojure-mode-map cider-repl-mode-map)
  :infix "r"
  "" '(:ignore t :wk "repl")
  "n" '(cider-repl-set-ns :wk "set namespace")
  "q" '(cider-quit :wk "quit")
  "r" '(cider-restart :wk "restart")
  "R" '(cider-refresh :wk "refresh"))
