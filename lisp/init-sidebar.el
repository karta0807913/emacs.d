(defvar init-sidebar-previous-window nil
  "A variable store treemacs should open in which window.")

(defun treemacs-custom-setup ()
  (defun treemacs-visit-node-vsplit (&optional arg)
    "Open current file or tag by vertically splitting `next-window'.
Stay in current window with a prefix argument ARG."
    (interactive "P")
    (treemacs--execute-button-action
     :split-function #'evil-window-vsplit
     :file-action (find-file (treemacs-safe-button-get btn :path))
     :dir-action (dired (treemacs-safe-button-get btn :path))
     :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
     :tag-action (treemacs--goto-tag btn)
     :save-window arg
     :window  (or init-sidebar-previous-window (previous-window))
     :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

  (defun treemacs-visit-node-split (&optional arg)
    "Open current file or tag by vertically splitting `next-window'.
Stay in current window with a prefix argument ARG."
    (interactive "P")
    (treemacs--execute-button-action
     :split-function #'evil-window-split
     :file-action (find-file (treemacs-safe-button-get btn :path))
     :dir-action (dired (treemacs-safe-button-get btn :path))
     :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
     :tag-action (treemacs--goto-tag btn)
     :save-window arg
     :window  (or init-sidebar-previous-window (previous-window))
     :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

  (defun treemacs-visit-node-no-split (&optional arg)
    "Open current file or tag within the window the file is already opened in.
If the file/tag is no visible opened in any window use `next-window' instead.
Stay in current window with a prefix argument ARG."
    (interactive "P")
    (treemacs--execute-button-action
     :file-action (find-file (treemacs-safe-button-get btn :path))
     :dir-action (dired (treemacs-safe-button-get btn :path))
     :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
     :tag-action (treemacs--goto-tag btn)
     :save-window arg
     :ensure-window-split t
     :window  (or init-sidebar-previous-window (previous-window))
     :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))
  (define-key treemacs-mode-map "j" 'treemacs-next-line)
  (define-key treemacs-mode-map "k" 'treemacs-previous-line)
  (define-key treemacs-mode-map "J" 'treemacs-next-neighbour)
  (define-key treemacs-mode-map "K" 'treemacs-previous-neighbour)
  (define-key treemacs-mode-map "ov" 'treemacs-visit-node-vsplit)
  (define-key treemacs-mode-map "os" 'treemacs-visit-node-split)
  (define-key treemacs-mode-map "/" 'evil-ex-search-forward)
  (define-key treemacs-mode-map "n" 'evil-ex-search-next)
  (define-key treemacs-mode-map "N" 'evil-ex-search-previous)
  (define-key treemacs-mode-map "zz" 'evil-scroll-line-to-center)
  (define-key treemacs-mode-map "oh" nil)
  (define-key treemacs-mode-map "h" 'treemacs-collapse-parent-node)
  (define-key treemacs-mode-map "l" 'treemacs-RET-action)
  (define-key treemacs-mode-map (kbd "M-h") 'treemacs-root-up)
  (define-key treemacs-mode-map (kbd "M-l") 'treemacs-root-down)
  (define-key treemacs-mode-map (kbd "RET") 'treemacs-visit-node-no-split))

(add-hook 'treemacs-mode-hook 'treemacs-custom-setup)

(defun treemacs-show-project-root ()
  (interactive)
  (require 'treemacs)
  (setq init-sidebar-previous-window (get-buffer-window))
  (let ((project-root (my-get-project-root-directory))
        (file-name (buffer-file-name (current-buffer))))
    (if (or (not file-name) (not project-root))
        (treemacs-select-window)
      (when (not (treemacs-is-path file-name :in-workspace))
        (treemacs-add-project-to-workspace project-root (file-name-base project-root)))
      (treemacs-find-file)
      (treemacs-select-window))))


(defun treemacs-find-file-in-project ()
  (interactive)
  "Find file in treemacs workspace projects"
  (let* ((candidates (mapcar
                      (lambda (project)
                        (propertize
                         (treemacs-project->name project)
                         'project-path (treemacs-project->path project)))
                      (treemacs-workspace->projects
                       (treemacs-find-workspace-by-path (buffer-file-name)))))
         (target-project (when candidates (completing-read "Select Project" candidates))))
    (when target-project
      (let ((ffip-project-root (get-text-property 0 'project-path target-project)))
        (find-file-in-project)))))

(defun project-eshell-get-eshell-name ()
    (format "eshell<%s>" (my-get-project-root-directory)))

(defun my-project-eshell (&optional options)
  "Open a shell at the current project root

OPTIONS is a p-list which has the following option
reset-pwd: reset shell path to project root if t"
  (interactive)
  (let* ((project-root (my-get-project-root-directory))
         (eshell-buffer-name (project-eshell-get-eshell-name))
         (buffer (get-buffer eshell-buffer-name)))
    (unless buffer
      (setq buffer (eshell eshell-buffer-name))
      (with-current-buffer buffer
        (eshell/cd project-root))
      (switch-to-buffer nil))
    (when (plist-get options :reset-pwd)
      (with-current-buffer buffer
        (eshell/cd project-root)))
    (select-window
     (display-buffer-in-side-window buffer (list :side 'bottom)))))

(defun my-project-eshell-delete ()
  "Close window opened by my-project-eshell"
  (interactive)
  (my-project-eshell)
  (with-current-buffer (get-buffer (project-eshell-get-eshell-name))
    (delete-window)))

(provide 'init-sidebar)
