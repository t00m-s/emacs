;;; init-writing.el --- Writing packages. -*- lexical-binding: t -*-
(use-package olivetti
             :config
             (setq olivetti-body-width 80)
             (setq olivetti-style t))

(use-package org
             :ensure t
             :config
             (setq org-directory "~/notes/")
             (setq org-default-notes-file (concat org-directory "default-note.org"))
             (setq org-hide-leading-stars t)
             (setq org-todo-keywords
               '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
             (setq org-log-done 'time))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes/"))
  :config
  (org-roam-db-autosync-mode)  ;; Keep database synced
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n")
           :unnarrowed t))))

(use-package flyspell
  :hook (org-mode . flyspell-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init-writing)
;;; init-writing.el ends here
