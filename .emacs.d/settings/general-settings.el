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

(defun disable-bold-fonts ()
  (interactive)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list)))

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

(provide 'general-settings)
