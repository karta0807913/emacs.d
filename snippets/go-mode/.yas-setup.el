(defun yas-snippet-go-mode-within-ginkgo-context-block-p ()
  (when-let* ((_ (treesit-ready-p 'go))
              (_ (string-match "_test.go" (buffer-name)))
              (call-expression (treesit-parent-until
                                (treesit-node-at (point))
                                (lambda (node)
                                  (string= (treesit-node-type node)
                                           "call_expression"))))
              (first-expression (car (treesit-node-children call-expression "identifier")))
              (expression-type (treesit-node-type first-expression))
              (expression-text (treesit-node-text first-expression)))
    (and (string= expression-type "identifier")
         (or (string= expression-text "Describe")
             (string= expression-text "Context")
             (string= expression-text "When")))))

(defun yas-snippet-go-mode-within-ginkgo-block-p ()
  (when-let* ((_ (treesit-ready-p 'go))
              (_ (string-match "_test.go" (buffer-name)))
              (call-expression (treesit-parent-until
                                (treesit-node-at (point))
                                (lambda (node)
                                  (string= (treesit-node-type node)
                                           "call_expression"))))
              (first-expression (car (treesit-node-children call-expression "identifier")))
              (expression-type (treesit-node-type first-expression))
              (expression-text (treesit-node-text first-expression)))
    (and (string= expression-type "identifier")
         (or (string= expression-text "Describe")
             (string= expression-text "Context")
             (string= expression-text "It")
             (string= expression-text "By")
             (string= expression-text "BeforeEach")
             (string= expression-text "AfterEach")
             (string= expression-text "When")))))

(defun yas-snippet-go-mode-get-func-doc ()
  (when-let* ((eldoc (lsp-request
                    "textDocument/hover"
                    (lsp--text-document-position-params)))
            (contents (gethash "contents" eldoc)))
      (lsp--render-on-hover-content contents nil)))


(defun yas-snippet-go-mode-calling-function-p (&optional expand-key)
  (if (and (stringp expand-key)
           (string= (buffer-substring-no-properties
                     (- (point) (length expand-key)) (point))
                    expand-key))
      (let ((buf (current-buffer))
            (min (point-min))
            (max (point-max))
            (current-point (point)))
        (with-temp-buffer
          (insert-buffer-substring buf min (- current-point (length expand-key)))
          (save-excursion
            (insert-buffer-substring buf current-point max))
          (treesit-parser-create 'go)
          (yas-snippet--go-mode-calling-function-p)))
    (yas-snippet--go-mode-calling-function-p)))

(defun yas-snippet--go-mode-calling-function-p ()
  (save-excursion
    (back-to-indentation)
    (if (treesit-thing-at-point "call_expression" 'nested)
        t
      nil)))

(defun yas-snippet-go-mode-get-function-response-name (&optional expend-key)
  (if (and (stringp expend-key)
           (string= (buffer-substring-no-properties
                     (- (point) (length expend-key)) (point))
                    expend-key))
      (save-current-buffer
        (delete-region (- (point) (length expend-key)) (point))
        (let ((response (yas-snippet--go-mode-get-function-response-name)))
          (insert-before-markers expend-key)
          response))
    (yas-snippet--go-mode-get-function-response-name)))

(defun yas-snippet--go-mode-get-function-response-name ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let* ((line-node (treesit-thing-at-point "call_expression" 'nested))
           (selector (treesit-node-child line-node 1 "selector_expression")))
      (goto-char (- (treesit-node-start selector) 1))

      (let ((source-code (yas-snippet-go-mode-get-func-doc))
            (answer nil))
        (with-temp-buffer
          (insert source-code) ;; "func F(...) (...)" or "func (*a.A).F(...) (...)"
          (goto-char (point-max))
          (when (eq ?\) (char-before)) ;; if it has multiple return values
            (backward-sexp)) ;; "func F(...) |(...)" move to here
          (goto-char (+ (search-backward ")") 1)) ;; "func F(...)| error" move to here
          (backward-sexp) ;; "func F|(...) error" move to here
          (delete-region (point-min) (point)) ;; delete all function keyword and function name.
          (insert "func F") ;; append the function prefix back.
          (let* ((root (treesit-parser-root-node (treesit-parser-create 'go)))
                 (root-children (treesit-node-child root 0))
                 (func-children (treesit-node-children root-children))
                 (parameters (or
                              (car (cdr (seq-filter
                                         (lambda (node)
                                           (string= "parameter_list" (treesit-node-type node)))
                                         func-children)))
                              (seq-find (lambda (node)
                                          (string= "type_identifier" (treesit-node-type node)))
                                        func-children))))
            (cond
             ((string= "type_identifier" (treesit-node-type parameters))
              (when (string= "error" (treesit-node-text parameters))
                `(:node-start ,(treesit-node-start line-node)
                              :node-end ,(treesit-node-end line-node)
                              :names ("err"))))
             ((string= "parameter_list" (treesit-node-type parameters))
              (let ((parameters (seq-filter (lambda (node)
                                              (string= "parameter_declaration" (treesit-node-type node)))
                                            (treesit-node-children parameters))))
                `(:node-start ,(treesit-node-start line-node)
                  :node-end ,(treesit-node-end line-node)
                  :names ,(mapcar 'yas-snippet--go-mode-get-name-by-treesit-node parameters)))))))))))

(defun yas-snippet--go-mode-get-name-by-treesit-node (node)
  (let* ((children (treesit-node-children node))
         (name (treesit-node-text
                (or (seq-find (lambda (node)
                                (string= "identifier" (treesit-node-type node)))
                              children)
                    (seq-find (lambda (node)
                                (or
                                 (string= "type_identifier" (treesit-node-type node))
                                 (string= "qualified_type" (treesit-node-type node))))
                              children)))))
    (if (string= name "error")
        "err"
      name)))

(defun yas-snippet-go-mode-get-response-name-snippet (names count)
  (setq count (- count 1))
  (string-join
   (mapcar
    (lambda (name) (format "${%d:%s}" (setq count (+ count 1)) name)) names) ", "))