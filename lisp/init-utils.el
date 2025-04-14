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
    (message "Re-evaluating use-package declarations..."))
  (unless package-archive-contents
    (package-refresh-contents))
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
(provide 'init-utils)
;;; init-utils.el ends here
