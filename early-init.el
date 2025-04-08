;;; init.el --- Main file that will be loaded.  -*- lexical-binding: t; -*-

;; Defer garbage collection
(setq gc-cons-percentage 0.6)

;; Change default max size for reading processes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (menu-bar-lines       . 0)
        (tool-bar-lines       . 0)))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise       t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; No second pass of case-insensitive search over auto-mode-alist.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

;; Ignore warnings when compiling.
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Remove "For information about GNU Emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. Even if disabled with
;; `inhibit-startup-screen', it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; Performance hacks
(setq which-func-update-delay 1.0)
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-enable-at-startup nil)


;; Create the backup directory if it doesn't exist
(make-directory "~/.emacs-backups/" t)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
;; Make numbered backups
(setq version-control t)

;; Don't ask to delete excess backup versions
(setq delete-old-versions t)

;; Keep this many newest versions
(setq kept-new-versions 6)

;; Keep this many oldest versions
(setq kept-old-versions 2)

;; Use normal mode for backups (not making a new copy each time)
(setq backup-by-copying nil)
;; Put auto-save files in a specific directory

;; Create the auto-save directory if needed
(make-directory (concat user-emacs-directory "auto-saves/") t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves/") t)))

;; So we can detect this having been loaded
(provide 'early-init)
;;; early-init.el ends here
