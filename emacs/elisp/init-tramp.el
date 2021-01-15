
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(require 'tramp)

;; (use-package tramp
;;   :config
;;   (setq tramp-auto-save-directory "/tmp/tramp/")
;;   (setq tramp-chunksize 2000))

(provide 'init-tramp)
