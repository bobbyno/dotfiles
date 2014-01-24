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
                      rainbow-delimiters
                      clojure-mode
                      clojurescript-mode
                      clojure-test-mode
                      coffee-mode
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

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 165)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; disable bold fonts
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; turn off annoying follow symlink prompt
(setq vc-follow-symlinks t)

;; show line numbers
;; (global-linum-mode t)

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

(defun line-to-top-of-window ()
  (interactive)
  (recenter 0))

(defun h-window () (window-at 1 1))
(defvar h-buffer nil)

(defun j-window () (window-at 150 1))
(defvar j-buffer nil)

(defun k-window () (window-at 1 50))
(defvar k-buffer nil)

(defun l-window () (window-at 150 50))
(defvar l-buffer nil)

(defun switch-to-foo-window (window)
  (select-window (funcall window)))
(defun switch-to-h-window () (interactive) (switch-to-foo-window 'h-window))
(defun switch-to-j-window () (interactive) (switch-to-foo-window 'j-window))
(defun switch-to-k-window () (interactive) (switch-to-foo-window 'k-window))
(defun switch-to-l-window () (interactive) (switch-to-foo-window 'l-window))

(defun put-buffer-in-foo-window (window memory)
  (switch-to-foo-window window)
  (ido-switch-buffer)
  (remember-buffer memory))
(defun put-buffer-in-h-window () (interactive) (put-buffer-in-foo-window 'h-window 'h-buffer))
(defun put-buffer-in-j-window () (interactive) (put-buffer-in-foo-window 'j-window 'j-buffer))
(defun put-buffer-in-k-window () (interactive) (put-buffer-in-foo-window 'k-window 'k-buffer))
(defun put-buffer-in-l-window () (interactive) (put-buffer-in-foo-window 'l-window 'l-buffer))

(defun find-file-in-foo-window (window memory)
  (switch-to-foo-window window)
  (ido-find-file)
  (remember-buffer memory))
(defun find-file-in-h-window () (interactive) (find-file-in-foo-window 'h-window 'h-buffer))
(defun find-file-in-j-window () (interactive) (find-file-in-foo-window 'j-window 'j-buffer))
(defun find-file-in-k-window () (interactive) (find-file-in-foo-window 'k-window 'k-buffer))
(defun find-file-in-l-window () (interactive) (find-file-in-foo-window 'l-window 'l-buffer))

(defun put-recent-file-in-foo-window (window memory)
  (switch-to-foo-window window)
  (recentf-open-files)
  (remember-buffer))
(defun put-recent-file-in-h-window () (interactive) (put-recent-file-in-foo-window 'h-window 'h-buffer))
(defun put-recent-file-in-j-window () (interactive) (put-recent-file-in-foo-window 'j-window 'j-buffer))
(defun put-recent-file-in-k-window () (interactive) (put-recent-file-in-foo-window 'k-window 'k-buffer))
(defun put-recent-file-in-l-window () (interactive) (put-recent-file-in-foo-window 'l-window 'l-buffer))


(defun remember-buffer (memory)
  (set memory (buffer-name (window-buffer))))

(defun reopen-buffer (buffer window)
  (cond ((eq buffer nil))
        ((get-buffer buffer)
         (switch-to-foo-window window)
         (set-window-buffer nil (get-buffer buffer)))
        ((assoc buffer ido-virtual-buffers)
         (switch-to-foo-window window)
         (find-file (cdr (assoc buffer ido-virtual-buffers))))))

(defun four-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (split-window-horizontally)
  (switch-to-l-window)
  (split-window-horizontally)
  (map 'list #'reopen-buffer
       (list h-buffer j-buffer k-buffer l-buffer)
       (list 'h-window 'j-window 'k-window 'l-window)))

(defun do-command-from-here (shell-buffer cmd)
  (interactive)
  (save-some-buffers t)
  (switch-to-buffer-other-window shell-buffer)
  (goto-char (point-max))
  (insert cmd)
  (comint-send-input))

;; Clojure IDE setup
(defun clojure ()
  (interactive)
  (four-windows)
  (cider-jack-in)
  (shell)
  (shrink-window 10))

(global-set-key (kbd "<f9>") 'clojure)

;; window-related global key bindings

(windmove-default-keybindings 'control)

;; set the mac fn key to the hyper key
(setq ns-function-modifier 'hyper)

(global-set-key (kbd "C-c b") 'ido-switch-buffer-other-window)

(global-set-key (kbd "s-h") 'switch-to-h-window)
(global-set-key (kbd "s-j") 'switch-to-j-window)
(global-set-key (kbd "s-k") 'switch-to-k-window)
(global-set-key (kbd "s-l") 'switch-to-l-window)

(global-set-key (kbd "H-h") 'put-buffer-in-h-window)
(global-set-key (kbd "H-j") 'put-buffer-in-j-window)
(global-set-key (kbd "H-k") 'put-buffer-in-k-window)
(global-set-key (kbd "H-l") 'put-buffer-in-l-window)

(global-set-key (kbd "C-x H-h") 'find-file-in-h-window)
(global-set-key (kbd "C-x H-j") 'find-file-in-j-window)
(global-set-key (kbd "C-x H-k") 'find-file-in-k-window)
(global-set-key (kbd "C-x H-l") 'find-file-in-l-window)

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

;; add line numbers for c
(add-hook 'clojure-mode-hook (lambda () (linum-mode)))

;; nrepl config
;; enable eldoc in cider

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
