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
                      nrepl
                      ws-trim))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; theme and font aesthetics
(load-theme 'zenburn t)

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

;; turn off annoying follow symlink prompt
(setq vc-follow-symlinks t)

;; ispell
(setq-default ispell-program-name "aspell")

;; ws-trim
(global-ws-trim-mode t)
(set-default 'ws-trim-level 2)
(setq ws-trim-global-modes '(guess (not message-mode eshell-mode)))

;; Turn off auto-save. No more #foo# files
(setq auto-save-default nil)

;; No scratch message
(setq initial-scratch-message nil)

;; dirtree
(add-to-list 'load-path "~/.emacs.d/dirtree")
(require 'dirtree)
(require 'tree-mode)
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

