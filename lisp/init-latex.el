

(defun init-lsp-latex-mode ()
  (add-hook 'lsp-mode-hook (lambda ()
                             (setq tab-width 2)))
  (flycheck-mode +1)
  (flyspell-mode +1)

  (defun lsp-latex-toggle-xelatex ()
    (interactive)
    (let ((result
           (catch 'result
             (let ((index 0))
               (while (< index (length lsp-latex-build-args))
                 (if (equal (elt lsp-latex-build-args index) "-xelatex")
                     (throw 'result (delete '"-xelatex" lsp-latex-build-args)))
                 (setq index (1+ index))
                 )
               (throw 'result '["-pdf" "-interaction=nonstopmode" "-synctex=1" "-xelatex" "%f" ]))
             )))
      (setq lsp-latex-build-args result)))
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

  (lsp))

(with-eval-after-load "tex-mode"
  (require 'lsp-latex)
  (add-hook 'tex-mode-hook 'init-lsp-latex-mode)
  (add-hook 'latex-mode-hook 'init-lsp-latex-mode))

(provide 'init-latex)
