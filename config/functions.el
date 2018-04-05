(defadvice kill-region (before slick-cut activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun my/smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my/yank-primary-selection ()
  (interactive)
  (let ((primary (or (x-get-selection-value)
                     (x-get-selection))))
    (when primary
      (push-mark (point))
      (insert-for-yank primary))))

(defun my/calc-offset-on-org-level ()
  (* (or (org-current-level) 0) org-indent-indentation-per-level))

(defun my/org-fill-paragraph (&optional JUSTIFY)
  (let* ((fill-column (- fill-column (my/calc-offset-on-org-level))))
    (org-fill-paragraph JUSTIFY)))

(defun my/org-auto-fill-function ()
  (let* ((fill-column (- fill-column (my/calc-offset-on-org-level))))
    (org-auto-fill-function)))

(defun my/org-mode-hook ()
  (setq fill-paragraph-function   'my/org-fill-paragraph
        normal-auto-fill-function 'my/org-auto-fill-function))

(defun my/delete-file (filename)
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (delete-file filename)))

(defun my/rename-file ()
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir))
                            (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

(defun my/copy-file ()
  (interactive)
  (call-interactively 'write-file))

(defun my/company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun my/company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(defun my/switch-buffer (arg)
  (interactive "P")
  (with-persp-buffer-list () (counsel-ibuffer arg)))
