(setq-default hscroll-margin 2
                hscroll-step 1
                scroll-conservatively 1001
                scroll-margin 0
                scroll-preserve-screen-position t)

(use-package sublimity-scroll
  :straight sublimity
  :init (sublimity-mode 1))

(provide 'init-scrolling)
