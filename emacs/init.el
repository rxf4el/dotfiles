;;; init.el ---  -*- coding: utf-8 ; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; (let (file-name-handler-alist)
;;   ;; Ensure Doom is running out of this file's directory
;;   (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar conf:cache-dir (concat user-emacs-directory "cache/"))
(unless (file-exists-p conf:cache-dir)
  (make-directory conf:cache-dir))

(defvar --backup-directory (concat user-emacs-directory ".backup"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 5               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 25              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 250            ; number of keystrokes between auto-saves (default: 300)
      )

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; init file
;; (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/config-leaf.org"))
;;(provide 'init)
;;; init.el ends here
