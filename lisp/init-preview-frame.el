(defcustom preview-frame--make-parameters
  '((use-position . t)
    (user-size . t)
    (keep-ratio . t)
    (undecorated . t)
    (desktop-dont-save . t)
    (child-frame-border-width . 3)
    (internal-border-width . 3)
    (drag-internal-border . t)
    (z-group . above)
    (left . 0.5)
    (top . 0.5)
    (width . 0.8)
    (height . 0.8))
  ""
  :type '(choice alist))

(defvar preview-frame--frame nil)

(defun preview-frame--before-delete (frame)
  (when (frame-parameter frame 'preview-frame)
    (mapcar
     (lambda (buffer)
       (with-current-buffer buffer
         (preview-frame-mode 0)))
     (buffer-list))
    (when-let (parent-frame (frame-parameter frame 'parent-frame))
      (select-frame-set-input-focus parent-frame))))

(defun preview-frame--replace-current-window ()
  (interactive)
  (let ((frame (preview-frame-get-frame))
        (buffer (current-buffer)))
    (with-selected-window (frame-parameter frame 'previous-window)
      (switch-to-buffer buffer))
    (select-frame-set-input-focus (frame-parameter frame 'parent-frame))
    (delete-frame frame))
  (preview-frame-mode 0))

(defun preview-frame--rollback-frame ()
  (interactive)
  (let ((frame (preview-frame-get-frame)))
    (ignore-errors
      (xref-pop-marker-stack))
    (select-frame-set-input-focus (frame-parameter frame 'parent-frame))
    (delete-frame frame)))

(defun preview-frame-get-frame ()
  (unless (frame-live-p preview-frame--frame)
    (setq preview-frame--frame
          (make-frame (append preview-frame--make-parameters
                              `((parent-frame . ,(selected-frame))
                                (preview-frame . t))))))
  preview-frame--frame)

(defun preview-frame-show-definitions-function (fetcher alist)
  (let ((items (funcall fetcher)))
    (cond
     ((cdr items)
      (xref--show-xref-buffer fetcher alist))
     ((car items)
      (let ((marker (xref-location-marker (xref-item-location (car items))))
            (window (selected-window))
            (preview-frame (preview-frame-get-frame)))
        (set-frame-parameter preview-frame 'previous-marker (make-marker))
        (set-frame-parameter preview-frame 'previous-window window)
        (with-current-buffer (marker-buffer marker)
          (preview-frame-mode 1))
        (with-selected-frame preview-frame
          (mapcar 'delete-window (cdr (window-list)))
          (with-selected-window (car (window-list))
            (switch-to-buffer (marker-buffer marker))
            (goto-char (marker-position marker))
            (recenter 0)))
        (select-frame-set-input-focus preview-frame)
        (select-window (car (window-list))))))))

(defun preview-frame-preview ()
  (interactive)
  (let ((xref-show-definitions-function 'preview-frame-show-definitions-function))
    (my-evil-goto-definition)))

(add-hook 'delete-frame-functions 'preview-frame--before-delete)

;;;###autoload
(define-minor-mode preview-frame-mode
  "Toggle preview-frame-mode"
  :keymap '(([?\r] . preview-frame--replace-current-window)
            ("q" . preview-frame--rollback-frame))
  (when evil-mode
    (evil-make-overriding-map preview-frame-mode-map 'normal))
  (add-hook 'delete-frame-functions 'preview-frame--before-delete 0 t))

(provide 'init-preview-frame)
