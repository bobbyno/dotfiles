;; Clojure IDE setup

(defun clojure ()
  (interactive)
  (four-windows)
  (shell)
  (cider-jack-in)
  (shrink-window 10))

(global-set-key (kbd "<f9>") 'clojure)

;; window-related global key bindings

(windmove-default-keybindings 'control)

(global-set-key (kbd "C-c b") 'ido-switch-buffer-other-window)

(global-set-key (kbd "s-h") 'switch-to-h-window)
(global-set-key (kbd "s-j") 'switch-to-j-window)
(global-set-key (kbd "s-k") 'switch-to-k-window)
(global-set-key (kbd "s-l") 'switch-to-l-window)

(global-set-key (kbd "H-h") 'put-buffer-in-h-window)
(global-set-key (kbd "H-j") 'put-buffer-in-j-window)
(global-set-key (kbd "H-k") 'put-buffer-in-k-window)
(global-set-key (kbd "H-l") 'put-buffer-in-l-window)

(global-set-key (kbd "C-x H-h") 'find-file-in-h-window)
(global-set-key (kbd "C-x H-j") 'find-file-in-j-window)
(global-set-key (kbd "C-x H-k") 'find-file-in-k-window)
(global-set-key (kbd "C-x H-l") 'find-file-in-l-window)

(global-set-key (kbd "C-x v") 'split-window-right)
(global-set-key (kbd "C-x h") 'split-window-below)

;; customize indentation for midje facts
(require 'clojure-mode)
(define-clojure-indent
  (fact 'defun)
  (facts 'defun)
  (fact-group 'defun)
  (silent-fact 'defun)
  (future-fact 'defun)
  (tabular 'defun)
  (against-background 'defun)
  (error-let 'defun)
  (provided 0))

;; classic lambda for fn
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

(eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(fn\\>\\)"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) (make-char 'greek-iso8859-7 107))
                                 nil))))))

;; add line numbers for c
(add-hook 'clojure-mode-hook (lambda () (linum-mode)))

;; nrepl config
;; enable eldoc in cider

(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (paredit-mode +1)))

;; hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in switch-to-buffer (C-x b)
(setq cider-hide-special-buffers t)

;; enable paredit in the nrepl buffer
(add-hook 'nrepl-connected-hook 'paredit-mode)

;; specify the print length to be 100 to stop infinite sequences killing things.
(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

;; this overrides cider-repl-set-ns in order to add
;; nrepl-repl-requires-sexp. There's probably a better way to
;; accomplish this.
(defun custom-cider-repl-set-ns (ns)
    "Switch the namespace of the REPL buffer to NS."
    (interactive (list (cider-current-ns)))
    (if ns
        (with-current-buffer (cider-current-repl-buffer)
          (cider-eval (format "(in-ns '%s)" ns)
                             (cider-repl-handler (current-buffer)))
          (cider-eval-sync nrepl-repl-requires-sexp))
      (error "Sorry, I don't know what the current namespace is.")))

(add-hook 'nrepl-connected-hook 'rebind-cider-repl-set-ns)

(defun rebind-cider-repl-set-ns ()
  (interactive)
  (message "Rebinding C-c M-n to load defaults...")
  (define-key cider-mode-map (kbd "C-c M-n") 'custom-cider-repl-set-ns))

;; eval in repl
(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(global-set-key (kbd "H-e") 'cider-eval-expression-at-point-in-repl)

(provide 'clojure-settings)
