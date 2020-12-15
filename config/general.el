(defvar mfiano/dir-etc
  (file-name-as-directory
   (expand-file-name "etc" user-emacs-directory)))
(make-directory mfiano/dir-etc t)

(defvar mfiano/dir-org
  (file-name-as-directory (expand-file-name "~/Projects/Org")))
(defvar mfiano/font "Iosevka Slab 10")

(defvar mfiano/ignored-files
  '(".elc" ".pyc" ".exe" ".dll" ".fasl" ".o" ".so"
    ".7z" ".bz2" ".dmg" ".gz" ".iso" ".jar" ".rar" ".tar" ".tgz" ".xz" ".zip"
    ".db" ".sql" ".sqlite"
    ".DS_Store" "Thumbs.db"))

(setq custom-file (expand-file-name "custom.el" mfiano/dir-etc)
      default-input-method "TeX"
      delete-trailing-lines t
      display-line-numbers t
      display-time-default-load-average nil
      display-time-format "%I:%M%P"
      echo-keystrokes 0.1
      focus-follows-mouse t
      initial-scratch-message nil
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      jit-lock-defer-time nil
      locale-coding-system 'utf-8
      make-pointer-invisible t
      mouse-autoselect-window t
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3)
      mouse-yank-at-point t
      ring-bell-function 'ignore
      scroll-step 1
      select-enable-primary t
      split-width-threshold nil
      use-dialog-box nil
      user-full-name "Michael Fiano"
      user-mail-address "mail@mfiano.net"
      vc-follow-symlinks t
      x-select-enable-clipboard t
      x-select-enable-clipboard-manager nil)

(setq-default auto-save-default nil
              auto-save-list-file-prefix nil
              backup-inhibited t
              buffer-file-coding-system 'utf-8
              create-lockfiles nil
              cursor-in-non-selected-windows nil
              cursor-type 'hbar
              default-tab-width 2
              fill-column 80
              find-file-visit-truename t
              indent-tabs-mode nil
              indicate-empty-lines t
              read-file-name-completion-ignore-case t
              require-final-newline t
              sentence-end-double-space nil
              truncate-lines t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)
(display-time-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(fringe-mode 6)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist `(font . ,mfiano/font))

(load custom-file 'noerror)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package evil
  :init (setq evil-want-keybinding nil
              evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (setq evil-move-beyond-eol t)
  (advice-add #'evil-window-split :override #'mfiano/window-split)
  (advice-add #'evil-window-vsplit :override #'mfiano/window-vsplit))

(use-package evil-collection)

(defvar mfiano/window-big-p nil)

(defun mfiano/window-toggle-size ()
  (interactive)
  (setq mfiano/window-big-p
        (if (and mfiano/window-big-p
                 (assq ?_ register-alist))
            (ignore (ignore-errors (jump-to-register ?_)))
          (window-configuration-to-register ?_)
          (if (window-dedicated-p)
              (cl-letf* ((old-window-resize (symbol-function #'window-resize))
                         (old-window-max-delta (symbol-function
                                                #'window-max-delta))
                         ((symbol-function #'window-resize)
                          (lambda (window delta &optional horizontal _ignore
                                          pixelwise)
                            (funcall old-window-resize window delta horizontal
                                     t pixelwise)))
                         ((symbol-function #'window-max-delta)
                          (lambda (&optional window horizontal _ignore trail
                                             noup nodown pixelwise)
                            (funcall old-window-max-delta window horizontal t
                                     trail noup nodown pixelwise))))
                (maximize-window))
            (maximize-window))
          t)))

(defvar mfiano/cycle-sly-repls-buffers nil)
(defun mfiano/cycle-sly-repls ()
  (interactive)
  (let ((buffers (remove-if-not
                  (lambda (x)
                    (string-match-p "\\*sly-mrepl.\**" (buffer-name x)))
                  (buffer-list))))
    (when (set-difference buffers mfiano/cycle-sly-repls-buffers)
      (setq mfiano/cycle-sly-repls-buffers buffers))
    (setq mfiano/cycle-sly-repls-buffers
          (remove nil (append (cdr mfiano/cycle-sly-repls-buffers)
                              (cons (car mfiano/cycle-sly-repls-buffers) nil))))
    (when-let* ((next-buffer (first mfiano/cycle-sly-repls-buffers))
                (window (get-buffer-window next-buffer t)))
      (select-frame-set-input-focus (window-frame window))
      (select-window window)
      (set-window-point window (point-max))
      (evil-insert-state))))
