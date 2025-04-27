;;; init-writing.el --- Writing packages. -*- lexical-binding: t -*-

;; --- Org Mode Configuration ----------------------------------------
(use-package org
  :defer nil
  :init
  ;; Setup modules before loading org
  (add-to-list 'org-modules 'org-habit t)
  :hook ((org-mode . (visual-line-mode olivetti-mode auto-fill-mode)))
  :custom
  (org-directory "~/notes/")
  (org-default-notes-file (concat org-directory "default.org"))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-adapt-indentation t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (calendar-week-start-day 1)
  :config
  (add-hook 'before-save-hook #'org-update-modification-time))

;; --- Visual Enhancements for Org -----------------------------------

(use-package olivetti
  :custom
  (olivetti-body-width 80)
  (olivetti-style t))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; --- Org Roam ------------------------------------------------------

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/notes/"))
  :config
  (org-roam-db-autosync-mode)
  (setq org-capture-templates
        '(("n" "Note" entry
           (file org-read-file-name)
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:DATE_CREATED: %t\n:LAST_MODIFIED: %t\n:AUTHOR: Tommaso Soncin\n:END:\n\n"))))

;; --- Spellchecking and Language Detection -------------------------

(use-package flyspell
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode)))

(use-package guess-language
  :custom
  (guess-language-languages '(en it))
  :hook (text-mode . guess-language-mode))

(provide 'init-writing)
;;; init-writing.el ends here
