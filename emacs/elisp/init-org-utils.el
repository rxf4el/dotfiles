;; Org-Ref
(use-package org-ref
  :custom ((org-ref-ivy-cite-completion t)
           (org-ref-completion-library 'org-ref-ivy-cite))
  :config
  (require 'org-ref)
  (require 'org-ref-pdf)
  (require 'org-ref-bibtex)
  (require 'org-ref-url-utils)
  (setq reftex-default-bibliography "~/Dropbox/org/biblio/ref.bib"
        org-ref-default-bibliography '("~/Dropbox/org/biblio/ref.bib")
        org-ref-bibliography-notes "~/Dropbox/org/biblio/notes.org"
        org-ref-pdf-directory "~/Dropbox/org/biblio/papers/"))


    ;; tasks management
    (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
    (setq org-log-done t
          org-clock-idle-time nil
          org-todo-keywords (quote
                             ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                              (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)" "PHONE" "MEETING"))))

;; Org-Agenda
;; agenda & diary
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
(setq org-agenda-include-diary nil
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-1d"
      org-agenda-span 7
      show-week-agenda-p t
      org-agenda-timegrid-use-ampm 1
      org-agenda-inhibit-startup t
      org-agenda-files (quote
                        ("~/Dropbox/org/work.org"
                         "~/Dropbox/org/stuff.org"
                         "~/Dropbox/org/inbox.org")))

(use-package org-super-agenda
  :straight t
  :defer t
  :config
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due today"
                                         :deadline today)
                                  (:name "Important"
                                         :priority "A")
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                  (:name "Big Outcomes"
                                         :tag "bo")))
  (org-super-agenda-mode))

(use-package org-gcal
  :config
  :config
  (setq org-gcal-client-id "70155025522-sod2sd5j69fqbtajhnllgmgprhavspo5.apps.googleusercontent.com"
        org-gcal-client-secret "x58hbBKXDZ8JfdEdeDYYC8_u"
        org-gcal-fetch-file-alist '(("rxf4el@gmail.com" .  "~/Dropbox/org/inbox.org"))))

(use-package calfw
  :config
  (setq cfw:display-calendar-holidays nil
        calendar-week-start-day 1
        cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗))

(use-package calfw-org
  :bind ("C-c f" . cfw:open-org-calendar)
  :config
  (setq cfw:org-overwrite-default-keybinding t))

;; Org-Utils (need to port for the right place)
;; date insertion configuration
(setq org-expiry-created-property-name "CREATED"
org-expiry-inactive-timestamps t
org-todo-state-tags-triggers (quote
                              (("CANCELLED" ("CANCELLED" . t))
                               ("WAITING" ("WAITING" . t))
                               ("HOLD" ("WAITING") ("HOLD" . t))
                               (done ("WAITING") ("HOLD"))
                               ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                               ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                               ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-latex-pdf-process
'("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))


(provide 'init-org-utils)
