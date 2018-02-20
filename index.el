;;; index.el - main emacs config entry point
;;; Commentary: our main .emacs file, to use
;;; 1. ln -s ~/.emacs ~/[path_to_here]/index.el
;;; 2. ln -s ~/.emacs.home ~/[path_to_here]
;;; 3. [optional] link emacs private el for anything you don't want checked into github (job specific)

;; get's added automatically, handled by init.el
;; (package-initialize)

(load "~/.emacs.home/init.el")
(load "~/.emacs.home/defaults.el")
(load "~/.emacs.home/packages.el")
(load "~/.emacs.home/functions.el")
(load "~/.emacs.home/bindings.el")
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
    (which-key ivy-rich ivy-ric ivy_buffer_extend nyan-mode nyan-cat ivy sublimity-scroll sublimity git-link dumb-jump robe robe-mode magit projectile prettier-js flycheck-flow company-flow tide yaml-mode web-mode use-package smooth-scrolling sass-mode rjsx-mode paradox list-packages-ext less-css-mode jsx-mode flycheck find-things-fast company-go color-theme-sanityinc-tomorrow color-theme coffee-mode better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
