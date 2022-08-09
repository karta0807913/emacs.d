;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'python
  (setq lsp-pylsp-plugins-yapf-enabled t)
  (setq lsp-pylsp-plugins-rope-completion-enabled t)
  (setq lsp-pylsp-plugins-pycodestyle-enabled t)
  (defun lsp-python-install-save-hooks()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))
  (add-hook 'python-mode-hook #'lsp-python-install-save-hooks)
  (add-hook 'python-mode-hook #'flycheck-mode)
  (add-hook 'python-mode-hook #'lsp-deferred))

(provide 'init-python)
