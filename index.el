(package-initialize)

(org-babel-load-file "~/.emacs.d/init.org")
(org-babel-load-file "~/.emacs.d/defaults.org")
(org-babel-load-file "~/.emacs.d/packages.org")
(org-babel-load-file "~/.emacs.d/functions.org")
(org-babel-load-file "~/.emacs.d/bindings.org")
(org-babel-load-file "~/.emacs.d/orgmode.org")
(load "~/.emacs.private.el" 'missing-ok)
