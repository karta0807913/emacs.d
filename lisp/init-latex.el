(defun init-lsp-latex-mode ()
  (add-hook 'lsp-mode-hook (lambda ()
                             (make-local-variable tab-width)
                             (setq tab-width 2)))
  (flycheck-mode +1)
  (flyspell-mode +1)

  (require 'lsp-latex)
  (unless (lsp-workspace-root)
    (save-excursion
      (goto-char (point-min))
      (let ((comment (split-string (thing-at-point 'line t)))
            (add-p nil))
        (while comment
          (let ((target (car comment)))
            (when (or (string= target "xelatex") (string= target "program=xelatex"))
              (add-to-list 'lsp-latex-build-args "-xelatex" t)
              (setq add-p t)
              (setq comment nil)))
          (setq comment (cdr comment)))
        (when (not add-p)
          (setq lsp-latex-build-args (remove "-xelatex" lsp-latex-build-args))))))
  (setq lsp-latex-forward-search-executable "zathura")
  (setq lsp-latex-forward-search-args (vector "-x" "emacsclient -n +%{line} %{input}"  "--synctex-forward" "%l:1:%f" "%p" ))

  ;; for forward search
  (unless (and (fboundp 'server-running-p) (server-running-p))
    (server-start))

  (add-hook 'before-save-hook 'lsp-format-buffer 0 t)
  (electric-pair-mode +1)
  (lsp))

(defcustom latex-tikz-fc-gap "2cm"
  "Gap between flowchart node"
  :group 'init-latex
  :type 'string)

(with-eval-after-load "tex-mode"
  (add-hook 'tex-mode-hook 'init-lsp-latex-mode)
  (add-hook 'latex-mode-hook 'init-lsp-latex-mode)
  (defvar latex-tikz-previous-fc-node nil)

  (defun latex-tikz-shift-to-previous-fc-node (name style)
    (interactive
     (list
      (read-string "flowchart node name: ")
      (completing-read
       "Select Node Style: "
       '("startstop"
         "io"
         "process"
         "decision"))))
    (let ((direction (when latex-tikz-previous-fc-node
                       (completing-read
                        "Direction: "
                        '("north"
                          "south"
                          "east"
                          "west")))))
      (latex-tikz-insert-fc-node name style latex-tikz-previous-fc-node direction)))

  (defun latex-tikz-insert-fc-node (name style &optional parent direction)
    "insert tikz flowchart node on point"
    (interactive
     (list
      (read-string "flowchart node name: ")
      (completing-read
       "Select Node Style: "
       '("startstop"
         "io"
         "process"
         "decision"))))
    (insert (format "\\node[%s] (%s)" style name))
    (when direction
      (let ((shift-string (pcase direction
                            ("north" (concat "yshift=-" latex-tikz-fc-gap))
                            ("south" (concat "yshift=" latex-tikz-fc-gap))
                            ("east" (concat "xshift=" latex-tikz-fc-gap))
                            ("west" (concat "xshift=-" latex-tikz-fc-gap))
                            (default default))))
        (insert (format " at ([%s]%s.%s)" shift-string latex-tikz-previous-fc-node direction))))
    (insert "{")
    (save-excursion
      (insert "};"))
    (setq latex-tikz-previous-fc-node name)
    (evil-insert 1))

  (defun latex-tikz-draw-arrow (from to)
    (interactive
     (list (read-string "from: ")
           (read-string "to: ")))
    (if (yes-or-no-p "inert text? ")
        (let ((anchor (completing-read
                       "Anchor: "
                       '("north"
                         "south"
                         "east"
                         "west"))))
          (insert (format "\\draw [->] (%s) -- node[anchor=%s]{" from anchor))
          (save-excursion
            (insert (format "} (%s);" to)))
          (evil-insert 1))
      (insert (format "\\draw [->] (%s) -- (%s);" from to))))

  (general-create-definer latex-leader-def
    :prefix ","
    :states '(normal emacs)
    :keymaps 'latex-mode-map)
  (latex-leader-def
    "op" (lambda ()
           (interactive)
           (message "start latex building")
           (lsp-latex-build))
    "fw" 'lsp-latex-forward-search
    "fn" 'latex-tikz-insert-fc-node
    "fp" 'latex-tikz-shift-to-previous-fc-node
    "fd" 'latex-tikz-draw-arrow))

(provide 'init-latex)
