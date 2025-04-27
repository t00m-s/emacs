;;; init-theme.el --- Setting theme. -*- lexical-binding: t -*-
(use-package catppuccin-theme
  :config
  (custom-set-faces
   '(mode-line ((t (:background "#dce0e8" :foreground "#4c4f69" :box (:line-width 1 :color "#acb0be")))))
   '(mode-line-inactive ((t (:background "#e6e9ef" :foreground "#8c8fa1" :box (:line-width 1 :color "#bcc0cc")))))

   '(mode-line-buffer-id ((t (:foreground "#8839ef" :weight bold))))
   '(mode-line-emphasis ((t (:foreground "#fe640b" :weight bold))))
   '(mode-line-highlight ((t (:foreground "#40a02b" :box (:line-width 1 :color "#40a02b")))))
  ))


(use-package auto-dark
  :config
  (setq auto-dark-dark-mode-hook
        (list (lambda ()
                (set-catppuccin-flavor 'mocha)
                )))

  (setq auto-dark-light-mode-hook
        (list (lambda ()
                (set-catppuccin-flavor 'latte)
                )))
  (auto-dark-mode 1))
(provide'init-theme)
;;; init-theme.el ends here
