;; enables elpa / melpa
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(defun ensure-installed (&rest arguments)
  "Ensure one or more packages are installed"
  (dolist (p arguments)
     (unless (package-installed-p p)
       (package-install p))))

;; improved package menu, easy upgrades with paradox-upgrade-packages
(ensure-installed 'paradox)

;; https://github.com/technomancy/better-defaults
(ensure-installed 'better-defaults)
(require 'better-defaults)

;; mac/dvorok/kenesis specific keybindings
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'ctrl)
(setq mac-control-modifier 'meta)

; disables splash screen
(setq inhibit-splash-screen t)

;; WHTF I don't want things to flicker and flash and beep every damn time I hit `C-g`
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; makes everything use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

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
 '(package-selected-packages
   (quote
    (paradox flycheck yaml-mode web-mode use-package smooth-scrolling sass-mode pkg-info list-packages-ext less-css-mode jsx-mode js2-mode go-eldoc find-things-fast company-go color-theme-sanityinc-tomorrow color-theme coffee-mode better-defaults)))
 '(paradox-github-token t))

;; don't wrap lines - it's annoying for code
(setq-default truncate-lines t)

;; color theme for the pretty stuff
(ensure-installed 'color-theme)
(ensure-installed 'color-theme-sanityinc-tomorrow)

;; Turn on column-number mode
(column-number-mode)

(ensure-installed 'ansi-color)
(require 'ansi-color)
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

;; enables smooth scrolling
(ensure-installed 'smooth-scrolling)

;; allows you to find file based in a git defined directory
(ensure-installed 'find-things-fast)
(setq ftf-filetypes
      '("*.go"
        "*.jsx"
        "*.js"
        "*.coffee"
        "*.json"
        "*.md"
        ))



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

;; React
(add-to-list 'auto-mode-alist '("\\.jsx$" . javascript-mode))


;; CoffeeScript
(ensure-installed 'coffee-mode)
(add-to-list 'auto-mode-alist '("Piefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode))

;; LESS
(ensure-installed 'less-css-mode)

;; Sass
(ensure-installed 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; Yaml
(ensure-installed 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ES6 Compatible JS
(ensure-installed 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(ensure-installed 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defun add-js2-minor-mode-hook () (js2-minor-mode 1))
(add-hook 'web-mode-hook 'add-js2-minor-mode-hook)

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
(setenv "GOPATH" "/Users/chris/Work/go")

;; (ensure-installed 'flycheck)
(ensure-installed 'go-mode)
(ensure-installed 'company-go)
(ensure-installed 'go-eldoc)

(add-to-list 'load-path "/Users/chris/Work/go/src/github.com/dougm/goflymake")
;; (require 'go-flycheck)
(require 'go-flymake)

(add-to-list 'load-path "/Users/chris/Work/go/src/github.com/golang/lint/misc/emacs")
(require 'golint)

(add-hook 'go-mode-hook '(lambda () 
                           (add-to-path "/Users/chris/Work/go/bin")
                           (add-to-path "/usr/local/go/bin")
                           (if (not (string-match "go" compile-command))
                               (set (make-local-variable 'compile-command)
                                    "go build -v && go test -v $(go list ./...  | grep -v /vendor/) && go vet $(go list ./...  | grep -v /vendor/)"))
                           (add-hook 'go-mode-hook 'go-eldoc-setup)
                           (setq gofmt-command "goimports") 
                           (add-hook 'before-save-hook 'gofmt-before-save)
                           (set (make-local-variable 'company-backends) '(company-go))
                           (company-mode)
                           ;; (local-set-key [C-down] 'beginning-of-defun)
                           ;; (local-set_key [C-up] 'end-of-defun)
                           (local-set-key (kbd "M-.") 'godef-jump)))



;; custom keybindings
(global-set-key [f1] 'ftf-grepsource)
(global-set-key [f2] 'recompile)
(global-set-key [f3] 'next-error)

(global-set-key [f5] 'revert-buffer)
(global-set-key [f7] 'align-regexp)
(global-set-key [f8] 'call-last-kbd-macro)

(global-set-key [A-down] 'shrink-window)
(global-set-key [A-up] 'enlarge-window)
(global-set-key [A-right] 'enlarge-window-horizontally)
(global-set-key [A-left] 'shrink-window-horizontally)
(global-set-key [A-backspace] 'balance-windows)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-g") 'ftf-find-file)

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



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
