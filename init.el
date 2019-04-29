(defvar bootstrap-version)

(defvar mfiano/config-files
  '("general.el"
    "functions.el"
    "keybindings.el"
    "editor-base.el"
    "editor-ui.el"
    "editor-window.el"
    "editor-project.el"
    "apps.el"
    "dev-base.el"
    "dev-writing.el"
    "dev-shell.el"
    "dev-web.el"
    "dev-lisp.el"
    "dev-emacs-lisp.el"
    "dev-common-lisp.el"
    "dev-clojure.el"
    "dev-racket.el"))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t
        straight-cache-autoloads t))

(let ((path (file-name-as-directory
             (expand-file-name "config" user-emacs-directory)))
      (gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  (dolist (file mfiano/config-files)
    (load-file (expand-file-name file path))))
