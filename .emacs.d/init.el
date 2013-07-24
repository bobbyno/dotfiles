(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      exec-path-from-shell
                      zenburn-theme
                      clojure-mode
                      clojure-test-mode
                      nrepl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'zenburn t)

(server-start)

(defun set-mac-font (name size)
  (interactive
   (list (completing-read "font-name: "
                          (mapcar (lambda (p) (list (car p) (car p)))
                                  (font-family-list)) nil t)
         (read-number "size: " 12)))
  (set-face-attribute 'default nil
                      :family name
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height (* 10 size))
  (frame-parameter nil 'font))

(set-mac-font "Deja Vu Sans Mono" 17)

(setq-default ispell-program-name "aspell")
