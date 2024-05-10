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

(defmacro yas-snippet-without-prefix-buffer (expand-key &rest body)
  "this function only can be used in \"condition\""
  `(if (and (stringp ,expand-key)
           (string= (buffer-substring-no-properties
                     (- (point) (length ,expand-key)) (point))
                    ,expand-key))
      (let ((buf (current-buffer))
            (min (point-min))
            (max (point-max))
            (current-point (point)))
        (with-temp-buffer
          (insert-buffer-substring buf min (- current-point (length ,expand-key)))
          (save-excursion
            (insert-buffer-substring buf current-point max))
          (progn ,@body)))
    (progn ,@body)))

(defun yas-snippet-go-mode-can-expand-for-error-check-p (expand-key)
  (yas-snippet-without-prefix-buffer
   expand-key
   (treesit-parser-create 'go)
   (yas-snippet-go-mode-calling-function-p)))

(defun yas-snippet-go-mode-calling-function-p ()
  (save-excursion
    (back-to-indentation)
    (if (treesit-thing-at-point "call_expression" 'nested)
        t
      nil)))

(defun yas-snippet-go-mode-get-function-response-name ()
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
          (goto-char (+ (or (ignore-errors (search-backward ")")) 0) 1)) ;; "func F(...)| error" move to here
          (backward-sexp) ;; "func F|(...) error" move to here
          (delete-region (point-min) (point)) ;; delete all function keyword and function name.
          (insert "func F") ;; append the function prefix back.
          (let* ((root (treesit-parser-root-node (treesit-parser-create 'go)))
                 (root-children (treesit-node-child root 0))
                 (func-children (treesit-node-children root-children))
                 (parameters (yas-snippet-go-mode-get-parent-function-return-values (point))))
            (plist-put
             (plist-put parameters
                        :node-start (treesit-node-start line-node))
             :node-end (treesit-node-end line-node))))))))

(defun yas-snippet--go-mode-get-type-node-by-treesit-node (node)
  (car (nreverse (treesit-node-children node))))

(defun yas-snippet--go-mode-get-type-by-treesit-node (node)
  (treesit-node-text (yas-snippet--go-mode-get-type-node-by-treesit-node node)))

(defun yas-snippet--go-mode-get-name-node-by-treesit-node (node)
  (seq-find (lambda (node)
              (string= "identifier" (treesit-node-type node)))
            (treesit-node-children node)))

(defun yas-snippet--go-mode-get-name-by-treesit-node (node)
  (let* ((children (treesit-node-children node))
         (name (or (treesit-node-text
                    (yas-snippet--go-mode-get-name-node-by-treesit-node node))
                   (yas-snippet--go-mode-get-type-by-treesit-node node))))
    (if (string= name "error") "err" name)))

(defun yas-snippet-go-mode-get-default-value (node)
  (let ((type (treesit-node-type node))
        (name (treesit-node-text node)))
    (print type)
    (cond
     ((or (string= type "pointer_type")
          (string= type "slice_type")
          (string= type "channel_type")
          (string= type "function_type"))
      "nil")
     (t
      (cond
       ((or (string= "int" name)
             (string= "int64" name)
             (string= "int32" name))
        "0")
       ((string= "string" name)
        "\"\"")
       ((or (string= "byte" name)
            (string= "rune" name))
        "'0'")
       (t
        (format "%s{}" (treesit-node-text node))))))))

(defun yas-snippet-go-mode-get-response-name-snippet (names count)
  (setq count (- count 1))
  (string-join
   (mapcar
    (lambda (name) (format "${%d:%s}" (setq count (+ count 1)) name)) names) ", "))

(defun yas-snippet-go-mode-get-parent-function-return-values (pos)
  (when-let ((func-node (treesit-parent-until
                         (treesit-node-at pos)
                         (lambda (node)
                           (or (string= "function_declaration" (treesit-node-type node))
                               (string= "func_literal" (treesit-node-type node))))))
             (return-values (seq-find
                             (lambda (node)
                               (when-let ((type (treesit-node-type node)))
                                 (or (string= "parameter_list" type)
                                     (string= "pointer_type" type)
                                     (string= "qualified_type" type)
                                     (string= "type_identifier" type))))
                             (nreverse (treesit-node-children func-node)))))
    (cond
     ((string= "parameter_list" (treesit-node-type return-values))
      (let ((parameters (seq-filter
                         (lambda (node)
                           (string= "parameter_declaration" (treesit-node-type node)))
                         (treesit-node-children return-values))))
        (list
         :types (mapcar 'yas-snippet--go-mode-get-type-by-treesit-node parameters)
         :names (mapcar 'yas-snippet--go-mode-get-name-by-treesit-node parameters)
         :type-nodes (mapcar 'yas-snippet--go-mode-get-node-type-by-treesit-node parameters)
         :name-nodes (mapcar 'yas-snippet--go-mode-get-name-node-by-treesit-node parameters))
        ))
     (t
      (if (string= "error" (treesit-node-text return-values))
          (list
           :types '("error")
           :names '("err")
           :type-nodes return-values
           :name-nodes return-values)
        (list
         :types (list (treesit-node-text return-values))
         :names (list (treesit-node-text return-values))
         :type-nodes (list return-values)
         :name-nodes (list return-values)))))))