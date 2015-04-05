(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)

(autoload 'neotree "neotree" "New tree view" t)

(setq neo-smart-open t
      neo-show-header nil
      neo-banner-message nil
      neo-create-file-auto-open t
      neo-window-width 25)

(set-face-foreground 'neo-dir-link-face "SlateGray2")
(set-face-foreground 'neo-file-link-face "grey88")

(global-set-key (kbd "M-t") 'neotree-toggle)

(provide 'neotree-settings)
