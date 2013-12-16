(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
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
                      clojure-mode
                      clojure-test-mode
                      cider
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

;; whitespace tweaks
(global-ws-trim-mode t)
(set-default 'ws-trim-level 2)
(setq ws-trim-global-modes '(guess (not message-mode eshell-mode)))

;; Turn off auto-save. No #foo# for you.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; No scratch message
(setq initial-scratch-message nil)

;; dirtree
(add-to-list 'load-path "~/.emacs.d/dirtree")
(require 'dirtree)
(require 'tree-mode)
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; turn off auto fill
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; easily resize windows
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-x <down>") 'shrink-window)

;; find file in project
(global-set-key (kbd "C-x f") 'find-file-in-project)
(setq-default ffip-project-file '(".git" "project.clj" "pom.xml") )

;; window configs

;; Open one window for source, one for test, and a slightly smaller one for a repl
(defun clojure ()
  (interactive)
  (split-window-vertically)
  (split-window-horizontally)
  (cider-jack-in)
  (enlarge-window 5))

(global-set-key (kbd "<f9>") 'clojure)

;; customize indentation for midje facts
(require 'clojure-mode)
(define-clojure-indent
  (fact 'defun)
  (facts 'defun)
  (fact-group 'defun)
  (silent-fact 'defun)
  (future-fact 'defun)
  (tabular 'defun)
  (against-background 'defun)
  (error-let 'defun)
  (provided 0))

;; classic lambda for fn
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

(eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(fn\\>\\)"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) (make-char 'greek-iso8859-7 107))
                                 nil))))))

;; nrepl config
;; enable eldoc in clojure buffers

(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (paredit-mode +1)))

;; hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in switch-to-buffer (C-x b)
(setq cider-hide-special-buffers t)

;; enable paredit in the nrepl buffer
(add-hook 'nrepl-connected-hook 'paredit-mode)

;; specify the print length to be 100 to stop infinite sequences killing things.
(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

;; this overrides cider-repl-set-ns in order to add
;; nrepl-repl-requires-sexp. There's probably a better way to
;; accomplish this.
(defun custom-cider-repl-set-ns (ns)
    "Switch the namespace of the REPL buffer to NS."
    (interactive (list (cider-current-ns)))
    (if ns
        (with-current-buffer (cider-current-repl-buffer)
          (cider-eval (format "(in-ns '%s)" ns)
                             (cider-repl-handler (current-buffer)))
          (cider-eval-sync nrepl-repl-requires-sexp))
      (error "Sorry, I don't know what the current namespace is.")))

(add-hook 'nrepl-connected-hook 'rebind-cider-repl-set-ns)

(defun rebind-cider-repl-set-ns ()
  (interactive)
  (message "Rebinding C-c M-n to load defaults...")
  (define-key cider-mode-map (kbd "C-c M-n") 'custom-cider-repl-set-ns))

;; tabs are two spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))


