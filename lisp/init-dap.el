(defcustom dap-gutter-breakpoints-enable-face (propertize "⬤" 'face '(:foreground "red"))
  "face of dap-gutter breakpoints' face"
  :type 'propertize
  :group 'dap-gutter)

(defun dap-gutter--active-breakpoint-property ()
  "return the active display"
  (propertize " " 'display `((margin left-margin) ,dap-gutter-breakpoints-enable-face)))

(defun dap-gutter--add-active-breakpoint (line)
  "add breakpoint"
  (let ((ov (make-overlay line line)))
    (overlay-put ov 'before-string (dap-gutter--active-breakpoint-property))
    ;; add a property for delete
    (overlay-put ov 'dap-gutter t)))

(defun dap-gutter--breakpoints-update ()
  "display the breakpoints in margins"
  (interactive)
  (set-window-margins (get-buffer-window) 1 (cdr (window-margins)))
  (remove-overlays (point-min) (point-max) 'dap-gutter t)
  (mapcar (lambda (breakpoints) (dap-gutter--add-active-breakpoint (plist-get breakpoints :point)))
          (gethash (buffer-file-name) (dap--get-breakpoints))))

(with-eval-after-load 'dap-mode
  (unless (window-system)
    (add-hook 'dap-breakpoints-changed-hook 'dap-gutter--breakpoints-update)
    (dap-gutter--breakpoints-update)))

(provide 'init-dap)
