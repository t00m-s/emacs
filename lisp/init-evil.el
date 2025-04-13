;;; init-evil.el --- Vim keybindings -*- lexical-binding: t -*-
(use-package evil
  :ensure t
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
  (define-key evil-insert-state-map (kbd "C-a") 'mark-whole-buffer)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Optional: Make these work in other states too
  (define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit))

;; Add visual selection surround functionality
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  ;; Make it work like mini.surround in neovim with visual selection
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.3))

;; Set up general.el for leader key functionality
(use-package general
  :ensure t
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
    "sh" '(split-window-below :which-key "[S]plit [H]orizontal")
    "sv" '(split-window-right :which-key "[S]plit [V]ertical")

    "b" '(:ignore t :which-key "[B]uffer")
    "bd" '(kill-buffer :which-key "[D]elete")

    "n" '(:ignore t :which-key "[N]otes")
    "nc" '(org-capture :which-key "[C]apture")
    "na" '(org-agenda :which-key "[A]genda")
    "ns" '(:ignore t :which-key "[S]et")
    "nst" '((lambda () (interactive) (org-todo "TODO")) :which-key "[T]ODO")
    "nsn" '((lambda () (interactive) (org-todo "NEXT")) :which-key "[N]EXT")
    "nsw" '((lambda () (interactive) (org-todo "WAITING")) :which-key "[W]AITING")
    "nsd" '((lambda () (interactive) (org-todo "DONE")) :which-key "[D]ONE")
    "nsc" '((lambda () (interactive) (org-todo "CANCELLED")) :which-key "[C]ANCELLED")


    "p" '(:ignore t :which-key "[P]roject")
    "pv" '(dired-fullscreen :which-key "[V]iew")
    "SPC" '(ibuffer :which-key "Show Buffers")))
(provide 'init-evil)
;;; init-evil.el ends here
