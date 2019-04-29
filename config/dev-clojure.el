(use-package clojure-mode)

(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("<up>" . cider-repl-previous-input)
        ("<down>" . cider-repl-next-input))
  :config
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'cider-inspector-mode 'emacs)
  (push '(cider-docview-mode :width 0.5 :position right)
        popwin:special-display-config)
  (setq nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-clojure-cli-global-options "-A:dev:bench:trace"
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-repl-print-length 20
        cider-repl-history-display-duplicates nil
        cider-repl-history-size 1000
        cider-repl-history-file (expand-file-name "cider-repl-history" mfiano/dir-etc))
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(general-nmap
  :prefix ","
  :keymaps '(clojure-mode-map cider-repl-mode-map)
  "'" 'cider-jack-in
  "ed" 'cider-eval-defun-at-point
  "eD" 'cider-insert-defun-in-repl
  "ee" 'cider-eval-last-sexp
  "eE" 'cider-insert-last-sexp-in-repl
  "er" 'cider-eval-region
  "eR" 'cider-insert-region-in-repl
  "eu" 'cider-undef
  "gb" 'cider-pop-back
  "gg" 'cider-find-var
  "gn" 'cider-find-ns
  "ha" 'cider-apropos
  "hd" 'cider-doc
  "hg" 'cider-grimoire-web
  "hj" 'cider-javadoc
  "ii" 'cider-inspect
  "ir" 'cider-inspect-last-result
  "me" 'cider-macroexpand-1
  "mE" 'cider-macroexpand-all
  "nn" 'cider-browse-ns
  "nN" 'cider-browse-ns-all
  "rn" 'cider-repl-set-ns
  "rq" 'cider-quit
  "rr" 'cider-refresh
  "rR" 'cider-restart)
