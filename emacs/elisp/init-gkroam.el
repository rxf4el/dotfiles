(use-package gkroam
  :straight (gkroam
             :type git
             :host github
             :repo "Kinneyzhang/gkroam"
             :branch "develop")
  :hook (after-init . gkroam-mode)
  :init
  (setq gkroam-root-dir "~/Dropbox/org/gkroam/"
        gkroam-prettify-page-p t
        gkroam-show-brackets-p t
        gkroam-use-default-filename t
        gkroam-window-margin 0)
  :bind
  (:map gkroam-mode-map
   (("C-c r I" . gkroam-index)
    ("C-c r d" . gkroam-daily)
    ("C-c r D" . gkroam-delete)
    ("C-c r f" . gkroam-find)
    ("C-c r i" . gkroam-insert)
    ("C-c r n" . gkroam-dwim)
    ("C-c r e" . gkroam-link-edit)
    ("C-c r u" . gkroam-show-unlinked)
    ("C-c r p" . gkroam-toggle-prettify)
    ("C-c r t" . gkroam-toggle-brackets)
    ("C-c r R" . gkroam-rebuild-caches)
    ("C-c r g" . gkroam-update))))

(provide 'init-gkroam)
