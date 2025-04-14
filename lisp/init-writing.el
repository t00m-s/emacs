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
             (setq org-todo-keywords
               '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
             (setq org-log-done 'time)
             ;; Enable basic syntax highlighting in code blocks
             (setq org-src-fontify-natively t)
             (setq org-src-tab-acts-natively t)
	     (setq org-adapt-indentation t)
	     (setq org-hide-leading-stars t)
	     (setq org-hide-emphasis-markers t)
	     (setq org-pretty-entities t)
	     (setq org-src-fontify-natively t)
	     (setq org-src-tab-acts-natively t)
	     (setq org-edit-src-content-indentation 0)
	     ;; Auto-enable visual-line mode for org files
	     (add-hook 'org-mode-hook 'visual-line-mode)
             ;; Auto-enable Olivetti mode for org files
             (add-hook 'org-mode-hook 'olivetti-mode)
             ;; Automatically enable auto-fill-mode in org documents
             (add-hook 'org-mode-hook 'auto-fill-mode))

(use-package org-modern
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

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
  :hook
  (org-mode . flyspell-mode)
  (text-mode . flyspell-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Prettier bullets for org-mode
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))
(provide 'init-writing)
;;; init-writing.el ends here
