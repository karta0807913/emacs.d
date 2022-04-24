
(with-eval-after-load 'xref
  (define-key xref--xref-buffer-mode-map (kbd "j") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "k") 'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "RET") '(lambda ()
                                                        (interactive)
                                                        (xref-goto-xref t))))

(provide 'init-xref)