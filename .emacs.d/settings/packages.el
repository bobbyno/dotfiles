(require 'package)

(setq package-archives '(("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

(setq package-check-signature nil)

(package-initialize)

(defvar my-packages '(autopair
                      cider
                      clojure-mode
                      dockerfile-mode
                      elisp-slime-nav
                      elpy
                      enh-ruby-mode
                      idle-highlight-mode
                      ido-ubiquitous
                      jinja2-mode
                      markdown-mode
                      neotree
                      paredit
                      rainbow-delimiters
                      robe
                      smex
                      yaml-mode
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; manually installed packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "ws-trim-1.4")

(provide 'packages)
