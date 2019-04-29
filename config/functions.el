(defmacro fn (&rest body)
  `(lambda () ,@body))

(defmacro fn! (&rest body)
  `(lambda () (interactive) ,@body))

(defadvice kill-region (before slick-cut activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun mfiano/smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun mfiano/yank-primary-selection ()
  (interactive)
  (let ((primary (or (x-get-selection-value)
                     (x-get-selection))))
    (when primary
      (push-mark (point))
      (insert-for-yank primary))))

(defun mfiano/delete-file (filename)
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (delete-file filename)))

(defun mfiano/rename-file ()
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

(evil-define-command mfiano/window-split (&optional count file)
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if evil-split-window-below 'above 'below))
  (call-interactively
   (if evil-split-window-below
       #'evil-window-up
     #'evil-window-down))
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

(evil-define-command mfiano/window-vsplit (&optional count file)
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if evil-vsplit-window-right 'left 'right))
  (call-interactively
   (if evil-vsplit-window-right
       #'evil-window-left
     #'evil-window-right))
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))
