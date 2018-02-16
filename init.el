;; enables elpa / melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; improved package menu, easy upgrades with paradox-upgrade-packages
(use-package paradox)

;; https://github.com/technomancy/better-defaults
(use-package better-defaults
  :config
  (require 'better-defaults))
