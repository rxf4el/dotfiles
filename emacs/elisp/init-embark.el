(use-package embark
  :bind
  ("C-S-a" . embark-act)               ; pick some comfortable binding
  :config
  ;; For Selectrum users:
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult)
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'init-embark)
