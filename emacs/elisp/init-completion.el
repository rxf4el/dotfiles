;;; -*- lexical-binding: t -*-

(defun +yas-expand-or-company-complete ()
  (interactive)
  (or (yas/expand)
      (call-interactively #'company-indent-or-complete-common)))

(use-package yasnippet
  :config
  (let ((inhibit-message t)) (yas-reload-all))
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :init
  (yas-global-mode +1))

(use-package company
  :init (global-company-mode +1)
  :commands (company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-idle-delay 0
        company-show-numbers t
        company-minimum-prefix-length 2
        company-tooltip-limit 5
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '((company-capf
                              company-keywords
                              company-yasnippet)
                             (company-abbrev company-dabbrev))
        company-frontends '(company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend)))


(use-package consult
  ;; :straight (consult :type git :host github :repo "minad/consult" :branch "main")
  :init
  (fset 'multi-occur #'consult-multi-occur)
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  :bind
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("C-," . consult-line)
  :custom
  (consult-line-point-placement 'match-end))

(use-package selectrum-prescient
  :init (selectrum-mode)
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-history-length 100
        prescient-sort-length-enable nil
        prescient-filter-method '(literal regexp initialism)))

(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package company-prescient
  :init
  (company-prescient-mode 1))

(use-package helm
  :custom (helm-bibtex-full-frame nil)
  :config
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 30)
  (helm-autoresize-mode 1))


;; (use-package ivy-bibtex
;;   :config
;;   (setq bibtex-completion-notes-path "~/Dropbox/org/biblio/"
;;           bibtex-completion-bibliography (list "~/Dropbox/org/biblio/ref.bib")
;;           bibtex-completion-library-path "~/Dropbox/org/biblio/papers/"
;;           bibtex-completion-pdf-field "File"
;;           bibtex-completion-notes-template-multiple-files
;;           (concat
;;            "#+TITLE: ${title}\n"
;;            ;; "#+ROAM_KEY: cite:${=key=}\n"
;;            "* TODO Notes\n"
;;            ":PROPERTIES:\n"
;;            ":CUSTOM_ID: ${=key=}\n"
;;            ;; ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
;;            ":AUTHOR: ${author-abbrev}\n"
;;            ":JOURNAL: ${journaltitle}\n"
;;            ":DATE: ${date}\n"
;;            ":YEAR: ${year}\n"
;;            ":DOI: ${doi}\n"
;;            ":URL: ${url}\n"
;;            ":END: ${end}\n\n")))

(use-package deadgrep
             :bind
             (:map deadgrep-mode-map
		               ("w" . 'deadgrep-edit-mode))
             (:map deadgrep-edit-mode-map
		               ("C-x C-s" . 'deadgrep-mode)))

(provide 'init-completion)
