(package-initialize)

(org-babel-load-file "~/.emacs.d/init.org")
(load "~/.emacs.d/defaults.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/bindings.el")
(org-babel-load-file "~/.emacs.d/orgmode.org")
(load "~/.emacs.private.el" 'missing-ok)
