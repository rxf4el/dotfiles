(use-package isearch
  :straight (:type built-in)
  :config
  (setq  search-highlight t
         isearch-lax-whitespace t
         isearch-regexp-lax-whitespace nil
         serach-whitespace-regexp ".*?"
         isearch-lazy-highlight t
         isearch-lazy-count t
         lazy-count-prefix-format nil
         lazy-count-suffix-format " (%s/%s)"
         isearch-yank-on-move 'shift
         isearch-allow-scroll . 'unlimited))

(provide 'init-isearch)
