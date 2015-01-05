(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")

(setq elpy-test-runner 'elpy-test-pytest-runner)

;; disable highlighting indentation and showing virtualenv in the mode line
(setq elpy-modules
      '(elpy-module-eldoc
        elpy-module-flymake
        elpy-module-yasnippet
        elpy-module-sane-defaults))

(add-hook 'elpy-mode-hook
 (lambda ()
   (define-key elpy-mode-map (kbd "C-c C-v") 'python-flake8)
   (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)))

;; pretty lambda's
(add-hook 'python-mode-hook 'esk-pretty-lambdas)
(add-hook 'inferior-python-mode-hook 'esk-pretty-lambdas)

(provide 'python-settings)
