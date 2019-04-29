;;; feed reader

(define-local-keys elfeed-search-mode-map
  "*" '(elfeed-toggle-star :wk "star"))

(use-package elfeed
  :config
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  (setq-default elfeed-search-filter "@1-month-ago +unread")
  (setq elfeed-search-title-max-width 150
        elfeed-search-date-format '("%Y-%m-%d %I:%M%P" 20 :left)))

(use-package notmuch
  :config (evil-collection-init 'notmuch))

(use-package counsel-notmuch)

(define-local-keys elfeed-search-mode-map
  :infix "s"
  "" '(:ignore t :wk "show")
  "*" `(,(fn! (elfeed-search-set-filter "starred")) :wk "starred")
  "a" `(,(fn! (elfeed-search-set-filter "")) :wk "all")
  "u" `(,(fn! (elfeed-search-set-filter "+unread")) :wk "unread")
  "y" `(,(fn! (elfeed-search-set-filter "@1-year-ago")) :wk "past year")
  "m" `(,(fn! (elfeed-search-set-filter "@1-month-ago")) :wk "past month")
  "w" `(,(fn! (elfeed-search-set-filter "@1-week-ago")) :wk "past week")
  "d" `(,(fn! (elfeed-search-set-filter "@1-day-ago")) :wk "past day"))
