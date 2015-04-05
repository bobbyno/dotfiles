(defun switch-to-file (file)
   (switch-to-buffer (find-file-noselect file))
   (markdown-mode))

(defun on-deck ()
   (interactive)
   (switch-to-file (concat (getenv "HOME") "/Desktop/work in progress/TODO/0_on_deck.md")))

(defun backlog ()
   (interactive)
   (switch-to-file (concat (getenv "HOME") "/Desktop/work in progress/TODO/0a_high_priority_after_on_deck.md")))

(global-set-key (kbd "M-0") 'on-deck)
(global-set-key (kbd "M-\-") 'backlog)
