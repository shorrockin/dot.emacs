;; --------------------------------------------
;; core emacs configuration
;; --------------------------------------------

;; disables the splash screen
(setq inhibit-splash-screen t)

;; makes the frame title display thcurrent buffer
(setq frame-title-format '("%b"))

;; don't wrap lines - it's annoying for code
(setq-default truncate-lines t)

;; enables you to move between windows using shift-arrow
(windmove-default-keybindings)

;; mac/dvorok/kenesis specific keybindings
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'ctrl)
(setq mac-control-modifier 'meta)

;; enables always transient mark mode
(transient-mark-mode 1)

;; makes all tabes spaces - currently disabled
(setq tab-width 2)
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq default-indent-tabs-mode nil)
(setq javascript-auto-indent-flag nil)

;; enables parenthesis to be shown (i.e. matching)
(show-paren-mode t)

;; enables line highlighting
(global-hl-line-mode 0)
(set-face-attribute 'default nil :font "Inconsolata-g")

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

;; turn off the menu and toolbar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; start emacs full-screen
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq tramp-backup-directory-alist backup-directory-alist)

(defun add-to-path (entry)
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name entry)))
  (setq exec-path (append exec-path (list (expand-file-name entry)))))

(add-to-path "/usr/local/bin")

;; turns on abbrev mode always
(abbrev-mode 1)
(setq default-abbrev-mode t)

;; set's the load path for external libraries
(setq load-path (cons "~/.emacs.d/add-ons" load-path))
(setq load-path (cons "~/.emacs.d/add-ons/color-theme-6.6.0" load-path))
(setq load-path (cons "~/.emacs.d/add-ons/ruby-mode/" load-path))


;; --------------------------------------------
;; emacs ui add-ons
;; --------------------------------------------

;; enables the buffers to not jump like a crazy person
(require 'smooth-scrolling)

;; enables color theme extension
(require 'color-theme-tomorrow)
(color-theme-tomorrow-night)

;; enables colors in the compilation buffer
(require 'ansi-color)
(defun colorize-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-buffer)

;; --------------------------------------------
;; emacs functionality add-ons
;; --------------------------------------------

;; load's the line number, package
(require 'linum)

;; loads a package that allows you to search a project, which is defined by 
;; the .git file in whatever super directory.
(require 'git-find-file)

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

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

;; use a single buffer for dired mode
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
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


;; --------------------------------------------
;; language specific configuration
;; --------------------------------------------

;; web mode allows pretty formatting and tab indenting of web files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; ruby mode with all the other non-rb files we want to support
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;; enables yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; coffee script
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; enables textile mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; enables go (and other related useful golang stuff)
(require 'go-mode)
(require 'go-autocomplete)
(require 'go-eldoc) 
(setenv "GOPATH" "/Users/chrisshorrock/Work/go.code")
(add-hook 'go-mode-hook '(lambda () 
                           (add-to-path "/Users/chrisshorrock/Work/go.code/bin")
                           (add-to-path "/usr/local/go/bin")
                           (if (not (string-match "go" compile-command))
                               (set (make-local-variable 'compile-command)
                                    "go build -v && go test -v && go vet"))
                           (add-hook 'go-mode-hook 'go-eldoc-setup)
                           (setq gofmt-command "goimports") ;; go get code.google.com/p/go.tools/cmd/goimports
                           (add-hook 'before-save-hook 'gofmt-before-save)
                           (local-set-key (kbd "M-.") 'godef-jump)
                           (local-set-key [f2] 'godef-jump)))


;; tabs to spaces for various languages
;;   - html
;;   - javascript
(defun me-turn-off-indent-tabs-mode () (setq indent-tabs-mode nil))
(add-hook 'html-mode-hook 'me-turn-off-indent-tabs-mode)
(add-hook 'javascript-mode-hook 'me-turn-off-indent-tabs-mode)


;; --------------------------------------------
;; org-mode configuration
;; --------------------------------------------

;; set's up org mode with .org and .todo files.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo\\'" . org-mode))

;; make windmove work in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; use indentation instead of ** for sub-lists
(setq org-startup-indented t)

;; startup with everything expanded
(setq org-startup-folded 0)

;; switches to the predefined org directory
(defun notes ()
  (interactive)
  (find-file "~/Dropbox/org_notes"))


;; --------------------------------------------
;; custom key bindings
;; --------------------------------------------

(global-set-key [f1] 'rgrep)

(global-set-key [f2] 'find-tag)
(global-set-key [f3] 'pop-tag-mark)

(global-set-key [f5] 'revert-buffer)
(global-set-key [f7] 'align-regexp)
(global-set-key [f8] 'call-last-kbd-macro)

(global-set-key [A-down] 'shrink-window)
(global-set-key [A-up] 'enlarge-window)
(global-set-key [A-right] 'enlarge-window-horizontally)
(global-set-key [A-left] 'shrink-window-horizontally)
(global-set-key [A-backspace] 'balance-windows)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-g") 'git-find-file)


;; --------------------------------------------
;; custom functions
;; --------------------------------------------

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; --------------------------------------------
;; custom variables
;; --------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(coffee-cleanup-whitespace t)
 '(coffee-tab-width 2)
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(sgml-tag-alist (quote (("![" ("ignore" t) ("include" t)) ("!attlist") ("!doctype") ("!element") ("<%") ("!entity"))))
 '(show-paren-mode t))
