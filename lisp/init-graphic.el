(when (display-graphic-p)
  (with-eval-after-load 'evil
    (evil-define-key 'insert 'global (kbd "C-S-v") 'paste-from-x-clipboard)
    (evil-define-key 'visual 'global (kbd "C-S-c") 'copy-to-x-clipboard)
    (evil-define-key 'normal 'global "gp" 'preview-frame-preview))
  (setq select-enable-clipboard nil)
  (global-set-key (kbd "C-S-v") 'paste-from-x-clipboard)
  (global-set-key (kbd "C-S-c") 'copy-to-x-clipboard)
  (require-init 'init-preview-frame))

(provide 'init-graphic)
