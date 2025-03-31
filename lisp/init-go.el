(with-eval-after-load 'go-mode
  (require 'treesit)
  (local-require 'general)

  (defun go-mode--treesit-find-test-ginko-it-function-name-at-point ()
    (when-let* ((p (treesit-ready-p 'go))
                (call-expression
                 (treesit-parent-until
                  (treesit-node-at (point))
                  (lambda (node)
                    (when (string= (treesit-node-type node) "call_expression")
                      (let ((expression (car (treesit-node-children node))))
                        (and (string= (treesit-node-type expression) "identifier")
                             (or
                              (string= (treesit-node-text expression) "DescribeTable")
                              (string= (treesit-node-text expression) "It"))))))
                  t))
                ;; children is (identifier argument_list)
                (children (treesit-node-children call-expression))
                (argument_list (car (cdr children))))
      (treesit-node-text (nth 1
                              ;; children of argument_list
                              (treesit-node-children (car (cdr children)))))))

  (defun go-mode--regex-find-test-t-function-name-at-point ()
    (save-excursion
      (evil-backward-section-begin)
      (let ((function-text (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
            (run-args nil))
        (save-match-data
          (and (string-match "func Test\\([^(]+\\)(t \\*testing.T)" function-text)
               (match-string 1 function-text)
               (setq run-args (format "-run Test%s" (match-string 1 function-text)))))
        run-args)))

  (defun go-mode-treesit-buffer-hook ()
    (when (treesit-ready-p 'go)
      (treesit-parser-create 'go)))

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

  (defun go-mode-run-unit-test ()
    (interactive)
    (setq compile-command
          (let ((ginkgo-it (go-mode--treesit-find-test-ginko-it-function-name-at-point))
                (regex-t (go-mode--regex-find-test-t-function-name-at-point)))
            (cond
             (ginkgo-it
              (format "ginkgo --focus %s" ginkgo-it))
             (regex-t
              (format "go test %s" regex-t))
             (t
              "go test ./..."))))
    (my-compile))

  (general-create-definer golang-leader-def
    :prefix ","
    :states '(normal emacs)
    :keymaps 'go-mode-map)

  (golang-leader-def
    "op" 'go-mode-run-unit-test)

  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'go-mode-hook #'go-mode-lsp-custon-settings)

  (add-hook 'go-mode-hook #'go-mode-treesit-buffer-hook)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'go-mode-configure))

(provide 'init-go)
