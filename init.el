;;; bootstrap straight.el

(defvar bootstrap-version)
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
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      straight-cache-autoloads t)

;;; load config files

(defun my/load-config-files (&rest files)
  (let ((path (file-name-as-directory
               (expand-file-name "config" user-emacs-directory))))
    (dolist (file files)
      (load-file (expand-file-name file path)))))

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  (my/load-config-files
   "functions.el"
   "general.el"
   "keybindings.el"
   "apps.el"
   "editor-base.el"
   "editor-ui.el"
   "editor-project.el"
   "editor-window.el"
   "dev-base.el"
   "dev-writing.el"
   "dev-shell.el"
   "dev-web.el"
   "dev-lisp.el"
   "dev-emacs-lisp.el"
   "dev-common-lisp.el"
   "dev-clojure.el"
   "dev-racket.el"))
