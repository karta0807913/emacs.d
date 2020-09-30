(defun init-lsp-latex-mode ()
  (flycheck-mode +1)
  (flyspell-mode +1)
  (lsp)
  )

(with-eval-after-load "tex-mode"
  (require 'lsp-latex)
  (add-hook 'tex-mode-hook 'init-lsp-latex-mode)
  (add-hook 'latex-mode-hook 'init-lsp-latex-mode))

(provide 'init-latex)