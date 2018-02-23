;; enables elpa / melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(toggle-frame-maximized)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)

(setq debug-on-error t
      load-prefer-newer t
      uniquify-buffer-name-style 'forward
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      )
