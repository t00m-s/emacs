;;; init-options.el --- Various options -*- lexical-binding: t -*-
(delete-selection-mode   t) ;; Replace selected text when yanking
(global-so-long-mode     t) ;; Mitigate performance for long lines
(global-visual-line-mode t) ;; Break lines instead of truncating them
(global-auto-revert-mode t) ;; Revert buffers automatically when they change
(recentf-mode            t) ;; Remember recently opened files
(savehist-mode           t) ;; Remember minibuffer prompt history
(save-place-mode         t) ;; Remember last cursor location in file
(setq auto-revert-interval         1         ;; Refresh buffers fast
      auto-revert-verbose          nil       ;; Don't notify me about reverts
      echo-keystrokes              0.1       ;; Show keystrokes fast
      frame-inhibit-implied-resize 1         ;; Don't resize frame implicitly
      sentence-end-double-space    nil       ;; No double spaces
      recentf-max-saved-items      1000      ;; Show more recent files
      use-short-answers            t         ;; 'y'/'n' instead of 'yes'/'no' etc.
      save-interprogram-paste-before-kill t  ;; Save copies between programs
      history-length               25        ;; Only save the last 25 minibuffer prompts
      global-auto-revert-non-file-buffers t) ;; Revert Dired and other buffers
(provide 'init-options)
;;; init-options.el ends here
