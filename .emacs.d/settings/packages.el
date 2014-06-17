(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-ruby
                      starter-kit-js
                      starter-kit-bindings
                      starter-kit-eshell
                      exec-path-from-shell
                      zenburn-theme
                      markdown-mode
                      yaml-mode
                      rainbow-delimiters
                      clojure-mode
                      clojurescript-mode
                      clojure-test-mode
                      coffee-mode
                      cider
                      ws-trim
                      elpy
                      autopair
                      robe
                      enh-ruby-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'packages)
