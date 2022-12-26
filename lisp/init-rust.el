(with-eval-after-load 'rust-mode
  (defun lsp-rust-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun lsp-rust-set-custom-settings()
    (setq lsp-rust-analyzer-server-display-inlay-hints t))

  (general-create-definer rust-leader-def
    :prefix ","
    :states '(normal emacs)
    :keymaps 'rust-mode-map)

  (defun lsp-rust-run-tests ()
    (interactive)
    (save-excursion
      (evil-backward-section-begin)
      (let ((actions (remq nil
                           (mapcar
                            (lambda (ov)
                              (when-let ((lsp-original (overlay-get ov 'lsp-original)))
                                (get-text-property 0 'action lsp-original)))
                            (overlays-in (line-beginning-position) (line-end-position))))))
        (unless (= (length actions) 0)
          ;; TODO: user a stable method to find the target function.
          ;; normally, current function should be the last item.
          ;; however, it might change in the next lsp version
          (funcall-interactively (car (last actions)))))))

    (rust-leader-def
      "op" 'lsp-rust-run-tests)

  (add-hook 'rust-mode-hook #'lsp-rust-set-custom-settings)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'lsp-deferred))

(provide 'init-rust)