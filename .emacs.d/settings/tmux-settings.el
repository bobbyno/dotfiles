;; Remap escape sequences to correct keybindings
(defun tmux-term-setup-hook ()
  (define-key function-key-map "\e[1;3A" [M-up])
  (define-key function-key-map "\e[1;3B" [M-down])
  (define-key function-key-map "\e[1;3C" [M-right])
  (define-key function-key-map "\e[1;3D" [M-left])
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[1;2A" [S-up])
  (define-key function-key-map "\e[1;2B" [S-down])
  (define-key function-key-map "\e[1;2C" [S-right])
  (define-key function-key-map "\e[1;2D" [S-left]))

(add-hook 'term-setup-hook 'tmux-term-setup-hook)

(provide 'tmux-settings)
