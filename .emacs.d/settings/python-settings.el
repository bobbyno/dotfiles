(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

(setq-default elpy-default-minor-modes
              (remove 'highlight-indentation-mode elpy-default-minor-modes))

(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")

(add-hook 'elpy-mode-hook
 (lambda ()
   (define-key elpy-mode-map (kbd "C-c C-v") 'python-flake8)))

;; pretty lambda's
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(add-hook 'inferior-python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

(provide 'python-settings)
