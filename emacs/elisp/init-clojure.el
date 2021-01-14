;;; -*- lexical-binding: t -*-

(use-package clojure-mode
             :hook
             (clojure-mode . paredit-mode)
             :bind
             (:map
              clojure-mode-map
              ("/" . 'cljr-slash)
              ("C-c C-i" . 'cider-inspect-last-result))
             :config
             (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
             (require 'init-clojure-highlight-fix)
             :custom
             (clojure-toplevel-inside-comment-form t))

(use-package clj-refactor
             :hook (clojure-mode . clj-refactor-mode)
             :config
             (unbind-key "/" clj-refactor-map)
             (cljr-add-keybindings-with-prefix "C-c C-r")
             :custom
             (cljr-warn-on-eval t)
             (cljr-suppress-middleware-warnings t))

(use-package cider
             :commands (cider-jack-in cider-jack-in-cljs cider-jack-in-clj&cljs)
             :bind
             (:map
              cider-mode-map
              ("C-!" . 'cider-read-and-eval)
              ("M-." . 'cider-find-var)
              :map
              cider-repl-mode-map
              ("M-," . 'cider-repl-handle-shortcut)
              ("C-," . 'cider-repl-handle-shortcut))
             :init
             (setq-default cider-default-cljs-repl 'shadow)
             :custom
             (cider-font-lock-dynamically nil)
             (cider-font-lock-reader-conditionals nil)
             (cider-use-fringe-indicators t)
             (cider-prompt-for-symbol nil)
             (cider-save-file-on-load t)
             (cider-enhanced-cljs-completion-p nil)
             (cider-offer-to-open-cljs-app-in-browser nil)
             :config
             (unbind-key "M-." cider-mode-map)
             (unbind-key "C-c C-p" cider-mode-map)
             (setq cider-allow-jack-in-without-project t
                   cider-repl-buffer-size-limit 100000
                   cider-repl-tab-command #'indent-for-tab-command
                   cider-repl-pop-to-buffer-on-connect 'display-only
                   cider-repl-result-prefix ";; => "
                   cider-repl-require-ns-on-set t
                   cider-repl-use-pretty-printing t
                   cider--print-buffer-size (* 8 1024)
                   cider-print-quota (* 1024 1024)))


(provide 'init-clojure)
