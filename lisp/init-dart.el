(defun dart-mode-lsp-setup()
  (flycheck-mode +1)
  (electric-pair-mode +1)
  (lsp))

(add-hook 'dart-mode-hook #'dart-mode-lsp-setup)
(provide 'init-dart)
