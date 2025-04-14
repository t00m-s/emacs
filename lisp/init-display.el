;;; init-display.el --- Graphical changes from default. -*- lexical-binding: t -*-
(dolist (mode
         '(tool-bar-mode       ;; Remove toolbar
           scroll-bar-mode     ;; Remove scollbars
           menu-bar-mode       ;; Remove menu bar
           blink-cursor-mode)) ;; Solid cursor, not blinking
  (funcall mode 0))
(setq inhibit-startup-message           t       ;; No startup message
      inhibit-startup-echo-area-message t       ;; No startup message in echo area
      inhibit-startup-screen            t       ;; No default startup screen
      initial-buffer-choice             t       ;; *scratch* is default startup buffer
      initial-major-mode                'fundamental-mode
      ring-bell-function                'ignore ;; No bell
      display-time-default-load-average nil     ;; Don't show me load time
      scroll-margin                     0       ;; Space between top/bottom
      use-dialog-box                    nil)    ;; Disable dialog

(global-display-line-numbers-mode 1)
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)
(global-prettify-symbols-mode 1)

(setq-default mode-line-format
              '(" "
                mode-name
                " | "
                mode-line-buffer-identification
                " %l:%c "))
(provide 'init-display)
;;; init-display.el ends here
