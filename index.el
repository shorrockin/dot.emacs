(package-initialize)

(org-babel-load-file "~/.emacs.d/init.org")
(org-babel-load-file "~/.emacs.d/defaults.org")
(org-babel-load-file "~/.emacs.d/packages.org")
(org-babel-load-file "~/.emacs.d/functions.org")
(org-babel-load-file "~/.emacs.d/bindings.org")
(org-babel-load-file "~/.emacs.d/orgmode.org")
(load "~/.emacs.private.el" 'missing-ok)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(package-selected-packages
   (quote
    (evil-escape evil-leader evil-tutor evil nyan-mode dumb-jump yaml-mode web-mode company magit rjsx-mode git-link projectile counsel ivy no-littering find-things-fast sublimity which-key color-theme-sanityinc-tomorrow use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t :background "#eebb28"))))
