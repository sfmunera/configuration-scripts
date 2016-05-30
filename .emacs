;; set indentation level to 4 spaces
(setq c-default-style "linux"
      c-basic-offset 4)
(setq tab-width 4)

(setq inhibit-splash-screen t)

(setq auto-fill-mode 1)
(setq TeX-PDF-mode t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (python . t)
   (sh . t)
   (latex . t)
   ))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

; always use spaces, not tabs, when indenting
(setq indent-tabs-mode nil)

; don't show the startup screen
(setq inhibit-startup-screen t)

; number of characters until the fill column
(setq fill-column 140)

; display line numbers to the right of the window
(global-linum-mode t)
; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)

; use the "Subtle Hacker" color theme as a base for the custom scheme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-subtle-hacker)
