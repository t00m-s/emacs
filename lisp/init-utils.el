;;; init-utils.el --- Utility functions -*- lexical-binding: t -*-

(defun dired-fullscreen ()
  "Open dired in full screen, similar to oil.nvim"
  (interactive)
  (dired default-directory)
  (delete-other-windows))

(provide 'init-utils)
;;; init-utils.el ends here
