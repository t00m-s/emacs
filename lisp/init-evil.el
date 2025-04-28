;;; init-evil.el --- Vim keybindings -*- lexical-binding: t -*-
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq scroll-margin 8)
  (setq scroll-conservatively 101)
  (evil-mode 1)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-find-file)
  (evil-define-key 'normal dired-mode-map (kbd "<return>") 'dired-find-file)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-a") 'mark-whole-buffer)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)

  (evil-set-initial-state 'ibuffer-mode 'normal)
  (define-key evil-normal-state-map (kbd "S-h") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "S-l") 'next-buffer)

    ;; Fix typo commands (for :wq and :w)
  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Wq" 'evil-save-and-close)
  (evil-ex-define-cmd "WQ" 'evil-save-and-close)

    ;; X deletion does not save to register
  (define-key evil-normal-state-map "x" (lambda () (interactive) (evil-delete-char (point) (1+ (point)) ?_)))
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'ibuffer))

;; Add visual selection surround functionality
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "sa" 'evil-surround-region))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.3))

;; Set up general.el for leader key functionality
(use-package general
  :after evil
  :config
  (general-create-definer my-leader-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; Show which-key menu after a brief delay when SPC is pressed
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   "SPC" '(nil :which-key "Leader"))

  ;; Define some basic keybindings under the leader key
  (my-leader-def
    "s" '(:ignore t :which-key "[S]earch")
    "sf" '(find-file :which-key "[F]ile")
    "sn" '(org-tags-view :which-key "[S]earch [N]ote Tags")
    "sh" '(split-window-below :which-key "[S]plit [H]orizontal")
    "sv" '(split-window-right :which-key "[S]plit [V]ertical")

    "b" '(:ignore t :which-key "[B]uffer")
    "bd" '(kill-buffer :which-key "[D]elete")

    "n" '(:ignore t :which-key "[N]otes")
    "nc" '(org-capture :which-key "[C]apture")
    "na" '(org-agenda :which-key "[A]genda")
    "ns" '((lambda () (interactive) (org-todo)) :which-key "[S]et")

    "p" '(:ignore t :which-key "[P]roject")
    "pv" '(dired-fullscreen :which-key "[V]iew")
    "SPC" '(ibuffer :which-key "Show Buffers")))
(provide 'init-evil)
;;; init-evil.el ends here
