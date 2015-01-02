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

;; hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in switch-to-buffer (C-x b)
(setq nrepl-hide-special-buffers t)

;; show nrepl port in cider-repl buffer name
(setq nrepl-buffer-name-show-port t)

;; classic lambda for fn
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

(defun pretty-lambdas (mode)
  (font-lock-add-keywords
      mode `(("(\\(fn\\>\\)"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1)
                                                 (make-char 'greek-iso8859-7 107))
                                 'decompose-region))))))

(eval-after-load 'clojure-mode (lambda () (pretty-lambdas 'clojure-mode)))

;; add line numbers
(add-hook 'clojure-mode-hook (lambda () (linum-mode)))

(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (paredit-mode +1)
                             (autopair-mode 0)))

(require 'no-bold-fonts)
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (cider-turn-on-eldoc-mode)
            (disable-bold-fonts)
            (paredit-mode 1)
            (autopair-mode 0)
            (rainbow-delimiters-mode 1)))

(defun require-repl-friends ()
    (interactive)
    (nrepl-sync-request:eval
      "(clojure.core/apply clojure.core/require
     '[[clojure.repl :refer :all]
       [clojure.pprint :refer :all]
       [clojure.java.javadoc :refer (javadoc)]])"))

(defun custom-cider-repl-set-ns (ns)
    "Switch the namespace of the REPL buffer to NS."
    (interactive (list (cider-current-ns)))
    (if ns
        (with-current-buffer (cider-current-repl-buffer)
          (cider-eval (format "(in-ns '%s)" ns)
                             (cider-repl-handler (current-buffer)))
          (require-repl-friends))
      (error "Sorry, I don't know what the current namespace is.")))

(add-hook 'nrepl-connected-hook
          (lambda ()
            (dolist (ns '("user" "clojure.core"))
              (nrepl-sync-request:eval (format "(in-ns '%s)" ns))
              (require-repl-friends))
            ;; Also require repl friends when changing namespaces.
            (define-key cider-mode-map (kbd "C-c M-n") 'custom-cider-repl-set-ns)))

;; eval in repl
(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(global-set-key (kbd "H-e") 'cider-eval-expression-at-point-in-repl)

;; clj-scratch buffer
(defun buffer-exists? (name)
  (= 1 (length
        (delq nil
              (mapcar
               (lambda (buf) (string-match name (buffer-name buf)))
               (buffer-list))))))

(defun clj-scratch ()
  "Create/retrieve a Clojure scratch buffer and switch to it.."
  (interactive)
  (switch-to-buffer (get-buffer-create "*clj-scratch*"))
  (clojure-mode)
  (if (not (buffer-exists? "*cider-repl"))
      (cider-jack-in)))

(global-set-key (kbd "H-c") 'clj-scratch)

(provide 'clojure-settings)
