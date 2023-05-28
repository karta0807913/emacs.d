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

(defvar my-python-venv-directories
  '("~/.emacs.d/elpy/rpc-venv"
    "~/.venv")
  "Directories of python venv.")

(defun my-activate-python-venv (directory)
  "Activate python venv in DIRECTORY."
  (my-ensure 'pyvenv)
  (let* ((venv-dir directory)
         (python pyvenv-virtualenvwrapper-python))

    (setq python (and (executable-find python) (executable-find "python3")))
    (cond
     ((null python)
      (message "Python executable \"%s\" is missing." python))
     ((not (file-exists-p venv-dir))
      (message "Directory \"%s\" is missing." venv-dir))
     (t
      (let* ((pyvenv-virtualenvwrapper-python python))
        (pyvenv-activate venv-dir))))))

(defun my-select-python-venv-and-restart-elpy ()
  "Activate python venv."
  (interactive)
  (let* ((venv-dir (completing-read "Select Python venv: "
                                    my-python-venv-directories)))
    (when venv-dir
      (my-activate-python-venv venv-dir)
      (elpy-disable)
      (elpy-shell-kill-all)
      (elpy-enable))))

(provide 'init-python)
;;; init-python.el ends here
