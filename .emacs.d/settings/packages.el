(require 'package)

(setq package-archives '(("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

(setq package-check-signature nil)

(package-initialize)

(defvar my-packages '(exec-path-from-shell
                      zenburn-theme
                      markdown-mode
                      yaml-mode
                      rainbow-delimiters
                      clojure-mode
                      cider
                      ws-trim
                      elpy
                      autopair
                      robe
                      enh-ruby-mode
                      dockerfile-mode
                      jinja2-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'packages)
