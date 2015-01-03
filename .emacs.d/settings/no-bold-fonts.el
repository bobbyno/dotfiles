(defun disable-bold-fonts ()
  (interactive)
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list)))

(provide 'no-bold-fonts)
