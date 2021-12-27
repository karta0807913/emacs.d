(defun go-mode-lsp-custon-settings()
  (add-hook 'lsp-mode-hook (lambda ()
                            (lsp-register-custom-settings
                              '(("gopls.completeUnimported" t t)
                                ("gopls.staticcheck" t t)
                                ("gopls.completeUnimported" t t))
                              ))))

(defun go-mode-configure ()
  (set-locale-environment compile-command)
  (setq compile-command "go test ./...")
  (eval-after-load 'dap-mode (progn  (require 'dap-go) (dap-go-setup))))

(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'go-mode-hook #'go-mode-lsp-custon-settings)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-hook 'go-mode-hook #'go-mode-configure)

(provide 'init-go)
