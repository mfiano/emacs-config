(setq user-full-name "Michael Fiano"
      user-mail-address "mail@michaelfiano.com")

(defvar my/dir-etc
  (file-name-as-directory
   (expand-file-name "etc" user-emacs-directory)))
(make-directory my/dir-etc t)

(defvar my/dir-org (file-name-as-directory (expand-file-name "~/dev/org")))

(defvar my/font "Iosevka Slab Medium 11")

(setq display-time-format "%I:%M%P"
      image-animate-loop t
      split-width-threshold nil
      jit-lock-defer-time nil
      custom-file (expand-file-name "custom.el" my/dir-etc)
      scroll-step 1
      mouse-wheel-scroll-amount '(3)
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-sel-mode t
      mouse-yank-at-point t
      make-pointer-invisible t
      default-buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8
      ring-bell-function 'ignore
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      use-dialog-box nil
      display-time-default-load-average nil
      x-select-enable-clipboard t
      echo-keystrokes 0.1
      vc-follow-symlinks t)

(setq-default cursor-type 'hbar
              cursor-in-non-selected-windows nil
              buffer-file-coding-system 'utf-8
              indicate-empty-lines t
              truncate-lines t
              fill-column 100
              switch-to-visible-buffer nil
              require-final-newline t
              sentence-end-double-space nil
              create-lockfiles nil
              read-file-name-completion-ignore-case t
              backup-inhibited t
              auto-save-default nil
              auto-save-list-file-prefix nil
              find-file-visit-truename t
              indent-tabs-mode nil
              default-tab-width 2)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)
(display-time-mode 1)
(column-number-mode 0)
(fringe-mode '(10 . 0))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist `(font . ,my/font))

(load custom-file 'noerror)

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(dolist (hook '(after-init-hook minibuffer-exit-hook))
  (add-hook hook (lambda () (setq gc-cons-threshold (* 1000 1000 10)))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
