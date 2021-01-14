;;; -*- lexical-binding: t -*-

(defun +yas-expand-or-company-complete ()
  (interactive)
  (or (yas/expand)
      (call-interactively #'company-indent-or-complete-common)))

(use-package yasnippet
             :bind
             (:map
              yas-keymap
              ("<escape>")
              ("RET" . 'yas-next-field-or-maybe-expand)
              ("<return>" . 'yas-next-field-or-maybe-expand)
              ("M-RET" . 'yas-expand-snippet)
              ("M-<return>" . 'yas-expand-snippet)
              ("S-<return>" . 'yas-prev-field)
              ("TAB")
              ("S-TAB")
              ("<tab>"))
             :config
             (let ((inhibit-message t)) (yas-reload-all))
             :init
             (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package company
             :hook (company-mode . company-tng-mode)
             :bind
             (:map company-mode-map
                   ("<tab>" . '+yas-expand-or-company-complete)
                   ("TAB" . '+yas-expand-or-company-complete))
             (:map company-active-map
                   ("<tab>" . 'company-complete-common-or-cycle)
                   ("TAB" . 'company-complete-common-or-cycle)
                   ("<escape>")
                   ("RET")
                   ("<return>")
                   ("SPC"))
             (:map company-template-nav-map
                   ("RET" . 'company-template-forward-field)
                   ("<return>" . 'company-template-forward-field)
                   ("TAB")
                   ("<tab>"))
             :init
             (require 'company-template)
             :hook
             ((prog-mode . company-mode)
              (conf-mode . company-mode)
              (eshell-mode . company-mode))
             :custom
             (company-tng-auto-configure nil)
             (company-frontends '(company-tng-frontend
                                  company-pseudo-tooltip-frontend
                                  company-echo-metadata-frontend))
             (company-begin-commands '(self-insert-command))
             (company-idle-delay 0.2)
             (company-tooltip-limit 10)
             (company-tooltip-align-annotations t)
             (company-tooltip-width-grow-only t)
             (company-tooltip-idle-delay 0.1)
             (company-minimum-prefix-length 3)
             (company-dabbrev-downcase nil)
             (company-abort-manual-when-too-short t)
             (company-require-match nil)
             (company-global-modes '(not dired-mode dired-sidebar-mode))
             (company-tooltip-margin 0))

(use-package ivy
             :init
             (ivy-mode 1)
             :custom
             (ivy-use-selectable-prompt t))

(use-package counsel
             :init
             (counsel-mode 1))

;; (use-package selectrum
;;   :init
;;   (selectrum-mode 1))
;;
(use-package prescient)
;; (use-package selectrum-prescient
;;   :init
;;   (selectrum-prescient-mode 1))
;; (use-package company-prescient
;;   :init
;;   (company-prescient-mode 1))

(use-package ivy-bibtex
  :config
  (setq bibtex-completion-notes-path "~/Dropbox/org/biblio/"
          bibtex-completion-bibliography (list "~/Dropbox/org/biblio/ref.bib")
          bibtex-completion-library-path "~/Dropbox/org/biblio/papers/"
          bibtex-completion-pdf-field "File"
          bibtex-completion-notes-template-multiple-files
          (concat
           "#+TITLE: ${title}\n"
           ;; "#+ROAM_KEY: cite:${=key=}\n"
           "* TODO Notes\n"
           ":PROPERTIES:\n"
           ":CUSTOM_ID: ${=key=}\n"
           ;; ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
           ":AUTHOR: ${author-abbrev}\n"
           ":JOURNAL: ${journaltitle}\n"
           ":DATE: ${date}\n"
           ":YEAR: ${year}\n"
           ":DOI: ${doi}\n"
           ":URL: ${url}\n"
           ":END: ${end}\n\n")))

(use-package deadgrep
             :bind
             (:map deadgrep-mode-map
		               ("w" . 'deadgrep-edit-mode))
             (:map deadgrep-edit-mode-map
		               ("C-x C-s" . 'deadgrep-mode)))

(provide 'init-completion)
