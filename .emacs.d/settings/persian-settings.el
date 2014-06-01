(defun persian ()
  "Create/retrieve a buffer in Persian text mode and switch to it."
  (interactive)
  (let ((buf (get-buffer-create "*persian*")))
    (switch-to-buffer buf)
    (set-input-method 'farsi-transliterate-banan)
    (setq buffer-face-mode-face '(:family "Monospace" :height 300))
    (buffer-face-mode)))

(provide 'persian-settings)
