(setq settings-path "~/.emacs.d/settings")

(add-to-list 'load-path settings-path)

(require 'packages)
(require 'general-settings)
(require 'window-settings)
(require 'clojure-settings)
(require 'python-settings)
(require 'python-flake8)
(require 'persian-settings)
(require 'ruby-settings)
(require 'tmux-settings)
(require 'neotree-settings)

(let ((user-settings (expand-file-name (concat settings-path "/" (getenv "USER") "-settings.el"))))
  (when (file-exists-p user-settings)
      (load user-settings)))

