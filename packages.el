;; color theme for the pretty stuff
(use-package color-theme-sanityinc-tomorrow
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night)
  (global-hl-line-mode 1)
  (custom-set-faces
   '(cursor ((t :background "#eebb28")))))

(use-package ansi-color)

(use-package which-key
  :ensure t
  :init
  (which-key-mode +1))

;; enables smooth scrolling, sublimity if you want it
(use-package sublimity
  :config
  (require 'sublimity)
  (require 'sublimity-scroll)
  (sublimity-mode 1))

;; allows you to find file based in a git defined directory
(use-package find-things-fast
  :bind
  ;; ("C-x g g" . ftf-find-file)
  ;; ("C-x C-g" . ftf-find-file)
  ("C-x g f" . ftf-grepsource)
  :config
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
          "*.yml"
          )))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume)
  (setq ivy-height 20))

 (use-package no-littering
   :demand t)

(use-package projectile
  :ensure t
  :bind
  ("C-x C-g" . projectile-find-file)
  ("C-x g g" . projectile-find-file)
  :config
  (projectile-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x))

;; used for quickly opening github links to line or region
(use-package git-link
  :bind
  ([f2] . git-link)
  :config
  (setq git-link-open-in-browser t))

;; React / JS / JSX
(use-package rjsx-mode
  :config
  (setq-default js2-strict-trailing-comma-warning nil) ;; ignore trailing commas
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode)))

(use-package magit)

(use-package company
  :config
  (global-company-mode))

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

;; CoffeeScript
(use-package coffee-mode
  :config
  (add-to-list 'auto-mode-alist '("Piefile" . coffee-mode))
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode)))

;; LESS
(use-package less-css-mode)

;; Sass
(use-package sass-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode)))

;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

;; ES6 Compatible JS
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode)))

;; Dumb-Jump - who needs TAGS when you can use this!?
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :bind
  ("M-." . dumb-jump-go)
  ("M-*" . dumb-jump-back))

(use-package nyan-mode
  :config
  (nyan-mode))
