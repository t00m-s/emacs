;;; init-utils.el --- Utility functions -*- lexical-binding: t -*-
(defun dired-fullscreen ()
  "Open dired in full screen, similar to oil.nvim"
  (interactive)
  (dired default-directory)
  (delete-other-windows))

(defun reload-config ()
  "Reload Emacs config and refresh packages. No drama."
  (interactive)
  (load-file user-init-file)
  (when (boundp 'package-archives)
    (package-refresh-contents))
  (when (fboundp 'use-package-report)
    (use-package-report))
  (package-install-selected-packages))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7) ;; update every 7 days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(defun set-catppuccin-flavor (flavor)
  "Set Catppuccin FLAVOR and reload theme."
  (setq catppuccin-flavor flavor)
  (load-theme 'catppuccin :no-confirm))

(defun update-org-modification-time ()
  "Update the LAST_MODIFIED property in org files when saving."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward ":LAST_MODIFIED:" nil t)
        (org-set-property "LAST_MODIFIED" (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun org-read-file-name ()
  "Return a file path for org-capture to use."
  (let* ((default-name "quick-note")
         (name (read-string "Note name (default: quick-note): ")))
    (expand-file-name (format "%s.org" (if (string-empty-p name) default-name name)) org-directory)))

(provide 'init-utils)
;;; init-utils.el ends here
