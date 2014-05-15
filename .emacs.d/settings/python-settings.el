(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

(setq-default elpy-default-minor-modes
              (remove 'highlight-indentation-mode elpy-default-minor-modes))

(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")

(provide 'python-settings)
