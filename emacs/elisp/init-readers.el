(use-package pdf-tools
  :straight t
  :bind (:map pdf-view-mode-map
         ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
         ("g"  . pdf-view-first-page)
         ("G"  . pdf-view-last-page)
         ("l"  . image-forward-hscroll)
         ("h"  . image-backward-hscroll)
         ("j"  . pdf-view-next-line-or-next-page)
         ("k"  . pdf-view-previous-line-or-previous-page)
         ("e"  . pdf-view-goto-page)
         ("t"  . pdf-view-goto-label)
         ("u"  . pdf-view-revert-buffer)
         ("al" . pdf-annot-list-annotations)
         ("ad" . pdf-annot-delete)
         ("aa" . pdf-annot-attachment-dired)
         ("am" . pdf-annot-add-markup-annotation)
         ("at" . pdf-annot-add-text-annotation)
         ("y"  . pdf-view-kill-ring-save)
         ("i"  . pdf-misc-display-metadata)
         ("s"  . pdf-occur)
         ("b"  . pdf-view-set-slice-from-bounding-box)
         ("r"  . pdf-view-reset-slice))
  :init
  (pdf-tools-install t t)
  :config
  (setq ;; pdf-misc-print-programm "/usr/bin/gtklp"
        pdf-misc-print-programm-args (quote ("-o media=A4" "-o fitplot"))
        pdf-view-display-size 'fit-page
        ;; pdf-view-use-imagemagick t
        pdf-view-midnight-colors '("white smoke" . "gray5")))

(use-package nov
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
            (add-hook 'nov-mode-hook
                      (lambda ()
                        (face-remap-add-relative 'variable-pitch
                                                 :family "Liberation Serif"
                                                 :height 1.2)))
            ;; (add-hook 'nov-mode-hook (lambda () (olivetti-mode 1)))
            (setq nov-text-width 72)))

(provide 'init-readers)
