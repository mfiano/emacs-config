(defvar my/emacs-config-org-file "emacs-config.org")
(defvar my/emacs-config-minimum-version "25.1")

(defun my/emacs-config-load ()
  (require 'ob-tangle)
  (let* ((path (file-name-directory (or load-file-name buffer-file-name)))
         (org-file (concat path my/emacs-config-org-file))
         (el-file (replace-regexp-in-string ".org" ".el" org-file)))
    (when (version< emacs-version my/emacs-config-minimum-version)
      (error "This configuration requires Emacs version >= %s"
             my/emacs-config-minimum-version))
    (when (or (not (file-exists-p el-file))
              (file-newer-than-file-p org-file el-file))
      (message "%s is newer than %s; tangling it." org-file el-file)
      (org-babel-tangle-file org-file))
    (load-file el-file)))

(my/emacs-config-load)
