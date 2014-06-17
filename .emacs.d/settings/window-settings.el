;; easily resize windows
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-x <down>") 'shrink-window)

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

(defun create-scratch-buffer ()
   "Create/retrieve a scratch buffer in text mode and switch to it."
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (text-mode))

(global-set-key (kbd "H-s") 'create-scratch-buffer)

(provide 'window-settings)
