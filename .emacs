
;; General settings

(defconst ha/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun ha/emacs-subdirectory (d) (expand-file-name d ha/emacs-directory))

;; Directory structure

;; Just to make sure that the following subdirectories exist:

(let* ((subdirs '("elisp" "backups" "snippets" "ac-dict" "templates"))
       (fulldirs (mapcar (lambda (d) (ha/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

;; Setting up the load path

;; Extra scripts go in =~/.emacs.d/elisp=:

(add-to-list 'load-path (ha/emacs-subdirectory "elisp"))

;; Package installation

;; First, let's add the Emacs repositories:

(require 'package)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(package-refresh-contents)

;; Use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Tabs vs Spaces

;; Let's just use spaces everywhere.

(setq-default indent-tabs-mode nil)
(setq tab-width 3)

;; Make tab key do indent first then completion.

(setq-default tab-always-indent 'complete)

;; Display Settings

;; Let's just get more screen space:

(setq initial-scratch-message "")
(setq visible-bell t)

(when (window-system)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

;; Fill-mode

;; Automatically wrapping when you get to the end of a line (or the
;; fill-region):

(use-package fill
  :bind ("C-c T f" . auto-fill-mode)
  :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :diminish auto-fill-mode)

;; Manipulate size of windows

;; Add Hydra as a package:

(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

;; And now it is possible to use arrow keys to resize windows :)
;; =<f9>-arrow=.

(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-splitter (global-map "<f9>")
  "splitter"
  ("<left>" hydra-move-splitter-left)
  ("<down>" hydra-move-splitter-down)
  ("<up>" hydra-move-splitter-up)
  ("<right>" hydra-move-splitter-right))

;; Jump to windows

;; Set up [[https://github.com/abo-abo/ace-window][ace-window]] mode:

(use-package ace-window
  :ensure t
  :init
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
    :diminish ace-window-mode)

;; Selecting a buffer

;; Use =IDO= for selecting buffers. =<f8>= shows the buffers in the
;; minibuffer. =S-<f8>= shows the buffers in another buffer:

(global-set-key (kbd "<f8>") 'ido-switch-buffer)
(global-set-key (kbd "S-<f8>") 'ibuffer)

;; TODO (Understand it better) Better jumping

;; This is a way to jump to chars or lines by using a tree approach
;; ([[https://github.com/abo-abo/avy][avy]]). Really cool!

(use-package avy
  :ensure t
  :commands avy-goto-word-1 avy-goto-char-1 avy-goto-line avy-goto-char-timer
  :bind
  ("C-c j"   . avy-goto-word-1)
  ("A-j"     . avy-goto-word-1)    ; The Mac Command key
  ("s-j"     . avy-goto-word-1)    ; The Command key on Linux
  ("A-h"     . avy-goto-char-2)
  ("s-h"     . avy-goto-char-2)
  ("C-c k k" . avy-goto-char-timer)
  ("A-J"     . avy-goto-char-timer)    ; The Mac Command key
  ("s-J"     . avy-goto-char-timer)    ; The Command key on Linux
  ("C-c k j" . avy-goto-word-1)
  ("C-c k c" . avy-goto-char-1)
  ("C-c k l" . avy-goto-line)
  ("C-c k p" . avy-pop-mark)
  ("A-,"     . avy-pop-mark))

;; Unfill paragraph

;; Unfilling a paragraph joins all the lines in a paragraph into a single
;; line with =M-Q=.

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; TODO Multiple cursors

;; This is the [[https://github.com/emacsmirror/multiple-cursors][multiple-cursors]] functionality:

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key
   (kbd "C-c C-.")
   (defhydra hydra-multiple-cursors ()
     "multiple-cursors"
     ("." mc/mark-all-dwim                   "all-dwim")
     ("C-." mc/mark-all-like-this-dwim       "all-like-dwim")
     ("n" mc/mark-next-like-this             "next")
     ("p" mc/mark-previous-like-this         "previous")
     ("a" mc/mark-all-like-this              "mark-all")
     ("N" mc/mark-next-symbol-like-this      "next-symbol")
     ("P" mc/mark-previous-symbol-like-this  "previous-symbol")
     ("A" mc/mark-all-symbols-like-this      "all-symbols")
     ("f" mc/mark-all-like-this-in-defun     "in-func")
     ("l" mc/edit-lines                      "all-lines")
     ("e" mc/edit-ends-of-lines              "end-lines"))))

;; Expand region

;; This is an extended version of the [[https://github.com/magnars/expand-region.el][expand-region]] taken from [[by ][here.]] To
;; select increasing regions around cursor, use =C-==.

(use-package expand-region
  :ensure t
  :config
  (defun ha/expand-region (lines)
    "Prefix-oriented wrapper around Magnar's `er/expand-region'.

Call with LINES equal to 1 (given no prefix), it expands the
region as normal.  When LINES given a positive number, selects
the current line and number of lines specified.  When LINES is a
negative number, selects the current line and the previous lines
specified.  Select the current line if the LINES prefix is zero."
    (interactive "p")
    (cond ((= lines 1)   (er/expand-region 1))
          ((< lines 0)   (ha/expand-previous-line-as-region lines))
          (t             (ha/expand-next-line-as-region (1+ lines)))))

  (defun ha/expand-next-line-as-region (lines)
    (message "lines = %d" lines)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line lines))

  (defun ha/expand-previous-line-as-region (lines)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line (1+ lines)))

  :bind ("C-=" . ha/expand-region))

;; Linux key bindings

;;    In Linux, use the Super key (Windows logo) for commonly used tasks.

(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-y") 'undo-tree-redo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-f") 'isearch-forward-regexp)

(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-w") 'bury-buffer)
(global-set-key (kbd "s-M-w") 'kill-this-buffer)

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)

(global-set-key (kbd "M-<up>") 'backward-page)
(global-set-key (kbd "M-<down>") 'forward-page)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)

;; Dired options

;; This is to use =find= when searching for files/directories with Dired:

(use-package find-dired
   :ensure t
   :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))

;; IDO (Interactively Do Things)

(use-package ido
  :ensure t
  :init  (setq ido-enable-flex-matching t
               ido-ignore-extensions t
               ido-use-virtual-buffers t
               ido-everywhere t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (add-to-list 'completion-ignored-extensions ".pyc"))

;; Now, it is more useful to see IDO results [[https://github.com/gempesaw/ido-vertical-mode.el][vertically]] (Nice!):

(use-package ido-vertical-mode
  :ensure t
  :init               ; I like up and down arrow keys:
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config
  (ido-vertical-mode 1))

;; TODO Helm

(use-package helm
  :ensure t
  :init
  (use-package helm-config))

;; TODO Auto insertion

;; It is possible to load pre-made templated in a blank file.

(use-package autoinsert
  :init
  (setq auto-insert-directory (ha/emacs-subdirectory "templates/"))
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1))

;; Auto insertion requires entering data for particular fields, and for
;; that Yasnippet is better, so in this case, we combine them:

(defun ha/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; Now, it is possible to define templates for auto-insert:

(use-package autoinsert
  :config
  (define-auto-insert "\\.el$" ["default-lisp.el" ha/autoinsert-yas-expand]))

;; TODO Yasnippets

;; [[https://github.com/capitaomorte/yasnippet][Yasnippets]] allows to load snippets of code into a file.

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (ha/emacs-subdirectory "snippets")))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq ispell-program-name "/usr/bin/aspell"
        ispell-dictionary "american" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-list-command "--list")

  (add-to-list 'ispell-local-dictionary-alist '(nil
                                                "[[:alpha:]]"
                                                "[^[:alpha:]]"
                                                "['‘’]"
                                                t
                                                ("-d" "en_US")
                                                nil
                                                utf-8)))

;; Line Numbers

;; Turn automatically =linum-mode= for programming modes:

(add-hook 'prog-mode-hook 'linum-mode)

;; Use =linum-relative= mode to insert relative line numbers.
;; To toggle between absolute and relative =linum-mode='s, use
;; =s-k= (Windows-k).

(use-package linum-relative
  :ensure t
  :config
  ;; Otherwise, let's take advantage of the relative line numbering:
  (defun linum-new-mode ()
    "If line numbers aren't displayed, then display them.
     Otherwise, toggle between absolute and relative numbers."
    (interactive)
    (if linum-mode
        (linum-relative-toggle)
      (linum-mode 1)))

  :bind ("s-k" . linum-new-mode))

;; Save File Position

;; Save the point position for every file, and restore it when that file
;; is reloaded.

(require 'saveplace)
(setq-default save-place t)
(setq save-place-forget-unreadable-files t)
(setq save-place-skip-check-regexp "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")

;; Visual Regular Expressions

;; Hightlights the searches while writing a regular expression.

;; Begin with =C-c r= then type the regexp. To see the highlighted
;; matches, type =C-c a= before you hit ‘Return’ to accept it.

(use-package visual-regexp
  :ensure t
  :init
  (use-package visual-regexp-steroids :ensure t)

  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace))

  ;; if you use multiple-cursors, this is for you:
  :config (use-package  multiple-cursors
            :bind ("C-c m" . vr/mc-mark)))

;; Flycheck

;; On-the-fly syntax checking.

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; We access stuff by loading the =etags= package:

(require 'etags)

;; Now, use the following keys:

;;    - M-. :: To find the tag at point to jump to the function’s
;;             definition when the point is over a function call. It is a
;;             dwim-type function.
;;    - M-, :: jump back to where you were.
;;    - M-? :: find a tag, that is, use the Tags file to look up a
;;             definition. If there are multiple tags in the project with
;;             the same name, use `C-u M-.’ to go to the next match.
;;    - =M-x tags-search= :: regexp-search through the source files
;;         indexed by a tags file (a bit like =grep=)
;;    - =M-x tags-query-replace= :: query-replace through the source files
;;         indexed by a tags file
;;    - =M-x tags-apropos= :: list all tags in a tags file that match a
;;         regexp
;;    - =M-x list-tags= :: list all tags defined in a source file

;; We can update the tags file whenever we save a file:

(use-package ctags-update
  :ensure t
  :config
  (add-hook 'prog-mode-hook  'turn-on-ctags-auto-update-mode)
  :diminish ctags-auto-update-mode)

;; Combining =imenu= with an IDO interface nicely lists the
;; headings/functions in the current buffer:

(use-package idomenu
  :ensure t
  :bind ("C-c i" . idomenu))

;; Apparently, Helm is quite good:

(use-package helm
  :bind (("C-c M-i" . helm-imenu)))

;; Code Block Folding
;; The Hide Show Minor mode allows us to fold all functions (hidden),
;; showing only the header lines. We need to turn on the mode, so
;; wrappers are in order:

(defun ha/hs-show-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-all))

(defun ha/hs-hide-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all))

(defun ha/hs-toggle-hiding ()
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding))

;; Rebindings:

(use-package hs-minor-mode
  :bind
  ("C-c T h" . hs-minor-mode)
  ("C-c h a" . ha/hs-hide-all)
  ("C-c h s" . ha/hs-show-all)
  ("C-c h h" . ha/hs-toggle-hiding))

;; Red Warnings

;; Various keywords (in comments) are now flagged in a Red Error font:

(add-hook 'prog-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

;; Paredit Mode
;; Keeps all parenthesis balanced in Lisp-oriented languages:

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;; Org Mode
;; See [[file:emacs-org.org][emacs-org-mode.el]].

(require 'init-org-mode)

;; Font Settings

;;    Syntax highlighting.

(global-font-lock-mode 1)

;; Let's try out these fonts:

(defvar ha/fixed-font-family
  (cond ((x-list-fonts "Hasklig")         "Hasklig")
        ((x-list-fonts "Source Code Pro") "Source Code Pro")
        ((x-list-fonts "Anonymous Pro")   "Anonymous Pro")
        ((x-list-fonts "M+ 1mn")          "M+ 1mn"))
  "My fixed width font based on what is installed, `nil' if not defined.")

;; Frame settings:

(when ha/fixed-font-family
  (set-frame-font ha/fixed-font-family)
  (set-face-attribute 'default nil :font ha/fixed-font-family :height 150)
  (set-face-font 'default ha/fixed-font-family))

(defvar ha/variable-font-tuple
  (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
        ((x-list-fonts "Verdana")         '(:font "Verdana"))
        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))
  "My variable width font available to org-mode files and whatnot.")

;; And now install a specific =color-theme=:

(use-package color-theme
  :ensure t
  :init (require 'color-theme)
  :config (use-package color-theme-sanityinc-tomorrow
           :ensure t))

;; For the Org mode's source code blocks, there are more settings:

(defun org-src-color-blocks-light ()
  "Colors the block headers and footers to make them stand out more for lighter themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
    ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-background
     ((t (:background "#FFFFEA"))))
   '(org-block
     ((t (:background "#FFFFEA"))))
   '(org-block-end-line
     ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))

   '(mode-line-buffer-id ((t (:foreground "#005000" :bold t))))
   '(which-func ((t (:foreground "#008000"))))))

(defun org-src-color-blocks-dark ()
  "Colors the block headers and footers to make them stand out more for dark themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background "#002E41"))))
   '(org-block-background
     ((t (:background "#000000"))))
   '(org-block
     ((t (:background "#000000"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background "#002E41"))))

   '(mode-line-buffer-id ((t (:foreground "black" :bold t))))
   '(which-func ((t (:foreground "green"))))))

(deftheme ha/org-theme "Sub-theme to beautify org mode")

;; Since I’m using the Powerline project, switching my Emacs color
;;   theme, requires me to call =powerline-reset= in order to get the
;;   colors to apply to the mode line.

;;   We put all of these requirements in a single function call:

(defun ha/change-theme (theme org-block-style)
  "Changes the color scheme and reset the mode line."
  (funcall theme)
  (funcall org-block-style)

  (let* ((ha/fixed-font-tuple (list :font ha/fixed-font-family))
         (base-font-color     (face-foreground 'default nil 'default))
         (background-color    (face-background 'default nil 'default))
         (primary-color       (face-foreground 'mode-line nil))
         (secondary-color     (face-background 'secondary-selection nil 'region))
         (base-height         (face-attribute 'default :height))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'ha/org-theme
                            `(org-agenda-structure ((t (:inherit default :height 2.0 :underline nil))))
                            `(org-verbatim ((t (:inherit 'fixed-pitched :foreground "#aef"))))
                            `(org-table ((t (:inherit 'fixed-pitched))))
                            `(org-block ((t (:inherit 'fixed-pitched))))
                            `(org-block-background ((t (:inherit 'fixed-pitched))))
                            `(org-block-begin-line ((t (:inherit 'fixed-pitched))))
                            `(org-block-end-line ((t (:inherit 'fixed-pitched))))
                            `(org-level-8 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-7 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-6 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-5 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-4 ((t (,@headline ,@ha/variable-font-tuple
                                                          :height ,(round (* 1.1 base-height))))))
                            `(org-level-3 ((t (,@headline ,@ha/variable-font-tuple
                                                          :height ,(round (* 1.25 base-height))))))
                            `(org-level-2 ((t (,@headline ,@ha/variable-font-tuple
                                                          :height ,(round (* 1.5 base-height))))))
                            `(org-level-1 ((t (,@headline ,@ha/variable-font-tuple
                                                          :height ,(round (* 1.75 base-height))))))
                            `(org-document-title ((t (,@headline ,@ha/variable-font-tuple :height 1.5 :underline nil)))))))

;; And the default startup goes to...night...unless I'm at work, and
;;   then we'll take the bright shiny theme.

(if (equal "smuneraa" user-login-name)
  (ha/change-theme 'color-theme-sanityinc-tomorrow-day
                   'org-src-color-blocks-light)
  (ha/change-theme 'color-theme-sanityinc-tomorrow-night
                   'org-src-color-blocks-dark))

;; Undo and Redo

;;   According to [[http://ergoemacs.org/emacs/emacs_best_redo_mode.html][this article]], I get better functionality than
;;   the =redo+= plugin (which I can't seem to get working well).

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-buffer-id ((t (:foreground "black" :bold t))))
 '(which-func ((t (:foreground "green"))) t))
