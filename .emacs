;; disables the splash screen
(setq inhibit-splash-screen t)

;; makes the frame title display thcurrent buffer
(setq frame-title-format '("%b"))

;; starts it up as a server
(server-start)

;; mac/dvorok/kenesis specific keybindings
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'ctrl)
(setq mac-control-modifier 'meta)

;; set's the load path for external libraries
(setq load-path (cons "~/.emacs.d/add-ons" load-path))
(setq load-path (cons "~/.emacs.d/add-ons/color-theme-6.6.0" load-path))
(setq load-path (cons "~/.emacs.d/add-ons/scala-trunk-06-12-09/" load-path))
(setq load-path (cons "~/.emacs.d/add-ons/yasnippet-0.6.1c/" load-path))
(setq load-path (cons "~/.emacs.d/add-ons/ruby-mode/" load-path))

(require 'ruby-mode)

(require 'less-mode)

;; enables yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; don't wrap lines - it's annoying for code
(setq-default truncate-lines t)

;; enables you to move between windows using shift-arrow
(windmove-default-keybindings)

;; enables the buffers to not jump like hyper red-heads
(require 'smooth-scrolling)

;; enables always transient mark mode
(transient-mark-mode 1)

;; makes all tabes spaces - currently disabled
(setq tab-width 2)
(setq default-tab-width 2)

;; coffee script
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; enables parenthesis to be shown (i.e. matching)
(show-paren-mode t)

;; enables textile mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; enables ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)
(ido-load-history)
(setq ido-save-directory-list-file "~/.emacs.d/ido.history")
(add-hook 'suspend-hook 'ido-save-history)

;; remove's c mode buffers from ido
(defun my-ido-ignore-buffers (name)
 "Ignore all c mode buffers -- example function for ido."
 (with-current-buffer name
   (cond ((or (derived-mode-p 'cvs-mode) (derived-mode-p
          'sql-interactive-mode)) nil)
         (t
          (string-match "^ ?\\*" name)))))
 
(setq-default ido-ignore-buffers '(my-ido-ignore-buffers) ido-auto-merge-work-directories-length -1)

;; enables color theme extension
(require 'color-theme)
(require 'color-theme-gruber-darker)
(color-theme-initialize)
(color-theme-gruber-darker)

;; enables line highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#111")
(set-face-foreground 'highlight nil) 
(set-face-foreground 'hl-line nil)

;; set's the load path for the abbreviations file
(setq abbrev-file-name "~/.emacs.d/abbrev-defs")
(setq save-abbrevs t)

;; makes everything use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; changes the default location for auto-save files
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(setq auto-save-file-name-transforms nil)
(setq tramp-auto-save-directory nil)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq tramp-backup-directory-alist backup-directory-alist)

;; load's the line number, package
(require 'linum)

;; turns on abbrev mode always
(abbrev-mode 1)
(setq default-abbrev-mode t)

;; use a single buffer for dired mode
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it'sloaded."
  ;; add other stuff here
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (joc-dired-single-buffer "..")))))
      ;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
  ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; scala support
(require 'scala-mode-auto)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq default-indent-tabs-mode nil)
(setq javascript-auto-indent-flag nil)

;; tables to spaces for scala mode
(defun me-turn-off-indent-tabs-mode () (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)

;; turn off tabs for html
(add-hook 'html-mode-hook 'me-turn-off-indent-tabs-mode)

;; no for javascript
(add-hook 'javascript-mode-hook 'me-turn-off-indent-tabs-mode)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/add-ons/scala-trunk-06-12-09/contrib/yasnippet")

(put 'downcase-region 'disabled nil)

(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))

;; smart tab augments the tab to do the "right" thing depending on where you
;; are in a document.
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

;; set's up everything needed to work with tramp.
;; (setq tramp-default-method "ssh")

;; set's up org mode with .org and .todo files.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo\\'" . org-mode))

;; configured cedet for use by ecb
(load-file "~/.emacs.d/add-ons/cedet-1.0/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; configure ecb
(add-to-list 'load-path "~/.emacs.d/add-ons/ecb-2.40")
(require 'ecb-autoloads)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/add-ons/rinari")
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

;; --------------------------------------------
;; All Custom Keybinding
;; --------------------------------------------

(fset 'switch-to-scratch
   [?\C-x ?b ?\( S-backspace ?* ?s ?c ?r ?a ?t ?c ?h ?* return])

(fset 'open-global-todo-list
   [?\C-x ?\C-f ?\C-x ?\C-f ?\C-a ?\C-k ?/ ?c ?h ?r backspace backspace backspace ?s ?c ?p ?: ?c ?h ?r ?i ?s ?@ ?s ?h ?o ?r ?r ?o ?c ?k ?i ?n ?. ?c ?o ?m ?: ?e ?m ?a ?c ?s ?. ?t ?o ?d ?o return])

(fset 'find-next-tag [?\C-u ?\M-.])
(fset 'tag-return [?\M-*])


(global-set-key [f1] 'rgrep)
(global-set-key [f2] 'find-tag)
(global-set-key [f3] 'find-next-tag)
(global-set-key [f4] 'tag-return)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f7] 'align-regexp)
(global-set-key [f8] 'call-last-kbd-macro)

(global-set-key [f9] 'open-global-todo-list)
(global-set-key [f10] 'switch-to-scratch)

(global-set-key [A-down] 'shrink-window)
(global-set-key [A-up] 'enlarge-window)
(global-set-key [A-right] 'enlarge-window-horizontally)
(global-set-key [A-left] 'shrink-window-horizontally)

(global-set-key (kbd "M-/") 'hippie-expand)

;; --------------------------------------------
;; All Custom Set Variables
;; --------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(coffee-cleanup-whitespace t)
 '(coffee-tab-width 2)
 '(ecb-compile-window-height nil)
 '(ecb-history-make-buckets (quote extension))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote never))
 '(ecb-source-path (quote ("~/Work/chimp/web")))
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(sgml-tag-alist (quote (("![" ("ignore" t) ("include" t)) ("!attlist") ("!doctype") ("!element") ("<%") ("!entity"))))
 '(show-paren-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
