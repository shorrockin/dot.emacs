(setq use-package-always-ensure t)

(setq temporary-file-directory "~/.emacs.d/tmp/")

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; mac/dvorok/kenesis specific keybindings
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'ctrl)
(setq mac-control-modifier 'meta)

;; remove white space on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; disables splash screen
(setq inhibit-splash-screen t)

;; WHTF I don't want things to flicker and flash and beep every damn time I hit `C-g`
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; makes everything use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; don't wrap lines - it's annoying for code
(setq-default truncate-lines t)

;; Turn on column-number mode
(column-number-mode)

;; allows you to move between buffers with shift-arrow
(windmove-default-keybindings)

;; Turn on highlight-line mode globally
(global-hl-line-mode)

;; damn you auto-fill mode
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; No blinking cursor
(blink-cursor-mode 0)

;; Default tab width of 2
(setq tab-width 2)
(setq default-tab-width 2)

(add-hook 'css-mode-hook '(lambda() (setq tab-width 2)))

(setq-default js2-basic-offset 2)
(setq-default css-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-attr-indent-offset 2)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
  (setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
  (setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
  (setq auto-save-list-file-prefix
      emacs-tmp-dir)
(setq create-lockfiles nil)
