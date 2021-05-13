(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun go-mode-lsp-custon-settings()
  (add-hook 'lsp-mode-hook (lambda ()
                            (lsp-register-custom-settings
                              '(("gopls.completeUnimported" t t)
                                ("gopls.staticcheck" t t)
                                ("gopls.completeUnimported" t t))
                              ))))

(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'go-mode-hook #'go-mode-lsp-custon-settings)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'yas-minor-mode)

(provide 'init-go)
