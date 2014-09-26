(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(setq make-backup-files nil)

(line-number-mode 1)
(column-number-mode 1)

(setq auto-fill-mode 1)
(setq TeX-PDF-mode t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
