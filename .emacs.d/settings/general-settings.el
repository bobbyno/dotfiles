(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; theme and font aesthetics
(load-theme 'zenburn t)

;; set the mac fn key to the hyper key
(setq ns-function-modifier 'hyper)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 170)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'no-bold-fonts)

;; calling this when init.el loads handles most cases,
;; but some modes re-enable bold fonts.
(disable-bold-fonts)

;; for those cases, use this keyboard shortcut:
(global-set-key (kbd "H-b") 'disable-bold-fonts)

;; indent regions
(global-set-key (kbd "H-i") 'indent-region)

;; turn off annoying follow symlink prompt
(setq vc-follow-symlinks t)

;; toggle line numbers
(global-set-key (kbd "C-c l") 'linum-mode)

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

;; turn off auto fill
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; make scripts executable after saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; remove annoying keyboard shortcuts
;;; show buffer list
(global-unset-key (kbd "C-x C-b"))

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

(provide 'general-settings)
