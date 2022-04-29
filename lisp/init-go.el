(with-eval-after-load 'go-mode
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

  (defun go-mode-configure ()
    (eval-after-load 'dap-mode (progn  (require 'dap-go) (dap-go-setup))))

  (general-create-definer golang-leader-def
    :prefix ","
    :states '(normal emacs)
    :keymaps 'go-mode-map)

  (golang-leader-def
   "op" (lambda ()
          (interactive)
          (save-excursion
            (evil-backward-section-begin)
            (let ((function-text (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))
                  (run-args "./..."))
              (save-match-data

                (if (and (string-match "func Test\\([^(]+\\)(t \\*testing.T)" function-text)
                          (match-string 1 function-text))
                    (setq run-args (format "-run Test%s" (match-string 1 function-text)))
                  )
                )
              (setq compile-command (format "go test %s" run-args))
              (my-compile)))))

  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'go-mode-hook #'go-mode-lsp-custon-settings)

  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'go-mode-configure))

(provide 'init-go)
