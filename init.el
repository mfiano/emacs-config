(defun my/load-config-files (&rest files)
  (let ((path (file-name-as-directory
               (expand-file-name "config" user-emacs-directory))))
    (dolist (file files)
      (load-file (expand-file-name file path)))))

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  (my/load-config-files
   "general.el"
   "functions.el"
   "packages.el"
   "keybindings.el"
   "dev-lisp.el"
   "dev-web.el"))
