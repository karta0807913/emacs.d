

(defun init-lsp-latex-mode ()
  (add-hook 'lsp-mode-hook (lambda ()
                             (setq tab-width 2)))
  (flycheck-mode +1)
  (flyspell-mode +1)

  (save-excursion
    (goto-char (point-min))
    (let (
          (result
           (split-string
            (buffer-substring (point) (progn (forward-line 1) (point)))))
          )
      (dolist (n result)
        (if (or (equal n "xelatex") (equal n "program=xelatex"))
            (setq lsp-latex-build-args
                  '["-pdf" "-interaction=nonstopmode" "-synctex=1" "-xelatex" "%f" ])))))
  (setq lsp-latex-forward-search-executable "zathura")
  (setq lsp-latex-forward-search-args (vector "-x" "emacsclient -n +%{line} %f"  "--synctex-forward" "%l:1:%f" "%p" ))

  (when (or (not (fboundp 'server-running-p)) (not (server-running-p)))
    (server-start))

  (lsp))

(with-eval-after-load "tex-mode"
  (require 'lsp-latex)
  (add-hook 'tex-mode-hook 'init-lsp-latex-mode)
  (add-hook 'latex-mode-hook 'init-lsp-latex-mode))

(provide 'init-latex)
