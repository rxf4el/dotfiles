(use-package hippie-expand
  :straight (:type built-in)
  :bind (("s-'" . hippie-expand))
  :config
  (setq hippie-expand-verbose t
        hippie-expand-try-functions-list
        '(yas/hippie-try-expand
          try-complete-file-name
          try-complete-file-name-partially
          try-expand-list
          try-expand-list-all-buffers
          try-expand-line
          try-expand-line-all-buffers
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(provide 'init-hippie-expand)
