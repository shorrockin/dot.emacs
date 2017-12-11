;; enables elpa / melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; improved package menu, easy upgrades with paradox-upgrade-packages
(use-package paradox)

;; https://github.com/technomancy/better-defaults
(use-package better-defaults)
(require 'better-defaults)

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

;; color theme for the pretty stuff
(use-package color-theme)
(use-package color-theme-sanityinc-tomorrow)

;; Turn on column-number mode
(column-number-mode)

(use-package ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;; enables smooth scrolling
(use-package smooth-scrolling)

;; allows you to find file based in a git defined directory
(use-package find-things-fast)
(setq ftf-filetypes
      '("*.go"
        "*.jsx"
        "*.js"
        "*.ts"
        "*.tsx"
        "*.rb"
        "*.coffee"
        "*.json"
        "*.md"
        "*.erb"
        "*.rb"
        ))

;; project is a set of tools for project interaction
;;   more details: http://batsov.com/projectile/
(use-package projectile
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-use-git-grep t)
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")

  :config
  (projectile-global-mode))

(use-package helm
  :bind (("M-x" . helm-M-x))
  :config
  (require 'helm-config)
  (helm-mode 1))


(use-package helm-projectile
  :init
  (setq helm-projectile-fuzzy-match nil)

  :config
  (helm-projectile-on))


;; Prompt before closing
(if window-system (setq confirm-kill-emacs 'yes-or-no-p))

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

(setq temporary-file-directory "~/.emacs.d/tmp/")

;; function to add it to the command path, as well as the emacs exec-path
(defun add-to-path (entry)
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name entry)))
  (setq exec-path (append exec-path (list (expand-file-name entry)))))
(add-to-path "/usr/local/bin")

;; React / JS / JSX
(use-package rjsx-mode)
(setq-default js2-strict-trailing-comma-warning nil) ;; ignore trailing commas
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))

(use-package magit)

;; Typescript / React / TSX
;; (use-package tide)

(use-package company
  :config (global-company-mode))

;;(defun setup-tide-mode ()
  ;; (interactive)
  ;; (tide-setup)
  ;; (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (eldoc-mode +1)
  ;; (tide-hl-identifier-mode +1)
  ;; (company-mode +1))

;; (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
;; (add-hook 'before-save-hook 'tide-format-before-save) ;; formats the buffer before saving
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package web-mode
  :init
  (defun web-mode-customization ()
    "Customization for web-mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (add-hook 'web-mode-hook 'web-mode-customization)

  :mode ("\\.html?\\'" "\\.erb\\'" "\\.hbs\\'"
         "\\.jsx?\\'" "\\.json\\'" "\\.s?css\\'"
         "\\.less\\'" "\\.sass\\'"))
(defun get-eslint-executable ()
  (let ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules")))
    (and root
         (expand-file-name "node_modules/eslint/bin/eslint.js"
                           root))))

;; CoffeeScript
(use-package coffee-mode)
(add-to-list 'auto-mode-alist '("Piefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode))

;; LESS
(use-package less-css-mode)

;; Sass
(use-package sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; Yaml
(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ES6 Compatible JS
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

;; Web Mode / Good For HTML and the like, not as good as RSJX for JS stuff

;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (defun add-js2-minor-mode-hook () (js2-minor-mode 1))
;; (add-hook 'web-mode-hook 'add-js2-minor-mode-hook)

;; Go
;; requires
;;   > go get -u github.com/nsf/gocode
;;   > go get -u github.com/rogpeppe/godef
;;   > go get -u golang.org/x/tools/cmd/goimports
;;   > go get -u github.com/dougm/goflymake
;;   > go get -u github.com/golang/lint/golint
;;
;; Resources:
;;   http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
;;
;; C-M-a   forward function (beginning-of-defun)
;; C-M-e   back function (end-of-defun)
;; M-.     jump forward to implementation
;; M-*     jump back to where you jumped forward
;;
;; (setenv "GOPATH" "/Users/chris/Work/go")

;; (use-package 'flycheck)
;; (use-package 'go-mode)
;; (use-package 'company-go)
;; (use-package 'go-eldoc)

;; (add-to-list 'load-path "/Users/chris/Work/go/src/github.com/dougm/goflymake")
;; (require 'go-flycheck)
;; (require 'go-flymake)

;; (add-to-list 'load-path "/Users/chris/Work/go/src/github.com/golang/lint/misc/emacs")
;; (require 'golint)

;; (add-hook 'go-mode-hook '(lambda ()
;;                            (add-to-path "/Users/chris/Work/go/bin")
;;                            (add-to-path "/usr/local/go/bin")
;;                            (if (not (string-match "go" compile-command))
;;                                (set (make-local-variable 'compile-command)
;;                                     "go build -v && go test -v $(go list ./...  | grep -v /vendor/) && go vet $(go list ./...  | grep -v /vendor/)"))
;;                            (add-hook 'go-mode-hook 'go-eldoc-setup)
;;                            (setq gofmt-command "goimports")
;;                            (add-hook 'before-save-hook 'gofmt-before-save)
;;                            (set (make-local-variable 'company-backends) '(company-go))
;;                            (company-mode)
                           ;; (local-set-key [C-down] 'beginning-of-defun)
                           ;; (local-set_key [C-up] 'end-of-defun)
;;                            (local-set-key (kbd "M-.") 'godef-jump)
;;                            (local-set-key (kbd "M-*") 'pop-tag-mark)))



;; Comment/uncomment shortcut
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

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

;; custom keybindings
;; (global-set-key [f1] 'ftf-grepsource)
;; (global-set-key [f2] 'recompile)
;; (global-set-key [f3] 'next-error)
(global-set-key (kbd "M-*") 'pop-tag-mark)
;; (global-set-key [f5] 'revert-buffer)
(global-set-key [f7] 'align-regexp)
(global-set-key [f8] 'call-last-kbd-macro)

(global-set-key [A-down] 'shrink-window)
(global-set-key [A-up] 'enlarge-window)
(global-set-key [A-right] 'enlarge-window-horizontally)
(global-set-key [A-left] 'shrink-window-horizontally)
(global-set-key [A-backspace] 'balance-windows)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-g") 'projectile-find-file)

(load "~/.emacs.private.el")

;; start emacs full-screen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(package-selected-packages
   (quote
    (magit helm-projectile helm projectile prettier-js flycheck-flow company-flow tide yaml-mode web-mode use-package smooth-scrolling sass-mode rjsx-mode paradox list-packages-ext less-css-mode jsx-mode flycheck find-things-fast company-go color-theme-sanityinc-tomorrow color-theme coffee-mode better-defaults))))
