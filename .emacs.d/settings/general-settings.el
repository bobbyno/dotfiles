(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(defun turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

(require 'uniquify)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      visible-bell t
      inhibit-startup-screen t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u"
      server-socker-dir "~/.emacs.d/server")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.

This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

(setq-default indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(eval-after-load 'hippie-exp
  '(progn
     (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
       (delete f hippie-expand-try-functions-list))

     ;; Add this back in at the end of the list.
     (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

;; Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; theme and font aesthetics
(load-theme 'zenburn t)

;; set the mac fn key to the hyper key
(setq ns-function-modifier 'hyper)

;; command is super
(setq ns-command-modifier 'super)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 170)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'no-bold-fonts)

;; calling this when init.el loads handles most cases,
;; but some modes re-enable bold fonts.
(disable-bold-fonts)

(global-unset-key (kbd "M-h"))

;; for those cases, use this keyboard shortcut:
(global-set-key (kbd "M-h b") 'disable-bold-fonts)

;; indent regions
(global-set-key (kbd "M-i") 'indent-region)

;; turn off annoying follow symlink prompt
(setq vc-follow-symlinks t)

;; toggle line numbers
(global-set-key (kbd "C-c l") 'linum-mode)

;; add a space for some margin in terminal mode
(setq linum-format "%d ")

;; ispell
(setq-default ispell-program-name "aspell")

;; whitespace tweaks
(global-ws-trim-mode t)
(set-default 'ws-trim-level 2)
(setq ws-trim-global-modes '(guess (not message-mode eshell-mode)))

;; Turn off auto-save. No #foo# for you.
(setq make-backup-files nil)
(setq auto-save-default nil)
;; ...but some buffers seem to still enable auto-save and backup...
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; No scratch message
(setq initial-scratch-message nil)

;; turn off auto fill in every way imaginable
(setq auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
;; call set-fill-column in a buffer interactively
;; when you actually need it.
(setq-default fill-column 99999)
(setq fill-column 99999)

;; find file in project
(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(setq-default ffip-project-file '(".git" "project.clj" "pom.xml") )
(add-to-list 'ffip-patterns "*\.cljs")

;; tabs are two spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; autopair (), "", {}, []
(require 'autopair)
(autopair-global-mode)

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
(put 'upcase-region 'disabled nil)

;; browse-url tweaks
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(global-set-key (kbd "M-g") 'browse-url-at-point)

;; answer yes or no questions with <y> or <n>
(fset 'yes-or-no-p 'y-or-n-p)

;; auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)

;; make scripts executable after saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; date utilities
(defun call-date (offset)
  (insert (shell-command-to-string (concat "echo -n $(date -v " offset "d +\"%A, %B %e %Y\")"))))

(defun today ()
  (interactive)
  (call-date "+0"))

(defun tomorrow ()
  (interactive)
  (call-date "+1"))

(defun yesterday ()
  (interactive)
  (call-date "-1"))

;; Disable most mouse wheel interactions
;; (mouse-wheel-mode -1)
;; (global-set-key [wheel-up] 'ignore)
;; (global-set-key [wheel-down] 'ignore)
(global-set-key [wheel-left] 'ignore)
(global-set-key [wheel-right] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [double-wheel-left] 'ignore)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
(global-set-key [triple-wheel-left] 'ignore)
(global-set-key [triple-wheel-right] 'ignore)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)


;; Turn off mouse interface early in startup to avoid momentary display
;; end starter-kit-bindings
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; starter-kit bindings

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Start a shell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'shell)

;; Start a new shell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (shell t)))

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i"
    '(lambda () (interactive)
       (if (not (eq 'Git (vc-backend buffer-file-name)))
           (vc-register)
         (shell-command (format "git add %s" buffer-file-name))
         (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Duplicates a line, works with undo, and doesn't mess with the cursor position
;; HT http://stackoverflow.com/a/998472/1054349
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "M-d") 'duplicate-line)

(define-key key-translation-map (kbd "M-8") (kbd "•"))
(define-key key-translation-map (kbd "M-p i") (kbd "π"))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(provide 'general-settings)
