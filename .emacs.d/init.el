(add-to-list 'load-path "~/.emacs.d/settings")
(require 'packages)
(require 'general-settings)
(require 'window-settings)
(require 'clojure-settings)

;; dirtree
(add-to-list 'load-path "~/.emacs.d/dirtree")
(require 'dirtree)
(require 'tree-mode)
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
