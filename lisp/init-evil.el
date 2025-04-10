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
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-a") 'mark-whole-buffer)
  (define-key evil-insert-state-map (kbd "C-a") 'mark-whole-buffer))

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
    "sf" '(find-file :which-key "[S]earch [F]ile")

    "b" '(:ignore t :which-key "[B]uffer")
    "bd" '(kill-buffer :which-key "[B]uffer [D]elete")

    "n" '(:ignore t :which-key "[N]otes")
    "nc" '(org-capture :which-key "[C]apture")
    "na" '(org-agenda :which-key "[A]genda")

    "pv" '(dired-fullscreen :which-key "[P]roject [V]iew")))
(provide 'init-evil)
;;; init-evil.el ends here
