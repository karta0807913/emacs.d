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

(defun yas-snippet-go-mode-ginkgo-check-error-p ()
  (and (string-match-p "_test.go$" (buffer-name))
       (yas-snippet-go-mode-within-ginkgo-block-p)))

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
  "This function only can be used in \"condition\".
The EXPAND-KEY indicates what is current's prefix key. Then create a temp buffer
And the code in BODY will be executed within the buffer."
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

(defun yas-snippet-go-mode-can-expand-for-return-p (expand-key)
  (yas-snippet-without-prefix-buffer
   expand-key
   (treesit-parser-create 'go)
   (and (not (yas-snippet-go-mode-calling-function-p)))))

(defun yas-snippet-go-mode-can-expand-for-error-return-p (expand-key)
  (yas-snippet-without-prefix-buffer
   expand-key
   (treesit-parser-create 'go)
   (and (yas-snippet-go-mode-calling-function-p)
        (yas-snippet-go-mode-parent-has-error-return-p (point)))))

(defun yas-snippet-go-mode-can-expand-for-error-check-p (expand-key)
  (and (yas-snippet-without-prefix-buffer
        expand-key
        (treesit-parser-create 'go)
        (and (yas-snippet-go-mode-calling-function-p)
             (not (yas-snippet-go-mode-parent-has-error-return-p (point)))))))

(defun yas-snippet-go-mode-can-expand-for-error-panic-p (expand-key)
  (and (string-match "\\^main.go\\$" (buffer-name))
       (yas-snippet-without-prefix-buffer
        expand-key
        (treesit-parser-create 'go)
        (and (yas-snippet-go-mode-calling-function-p)
             (not (yas-snippet-go-mode-parent-has-error-return-p (point)))))))

(defun yas-snippet-go-mode-parent-has-error-return-p (pos)
  (if (seq-find (lambda (type)
                  (string= type "error"))
                (plist-get (yas-snippet-go-mode-get-parent-function-return-values pos)
                           :types))
      t
    nil))

(defun yas-snippet-go-mode-get-call-expression-node (node)
  (cond
   ((string= "short_var_declaration" (treesit-node-type node))
    (when-let* ((right-value-children
                 (treesit-node-children
                  (seq-find (lambda (node)
                              (string= (treesit-node-type node) "expression_list"))
                            (nreverse (treesit-node-children node)))))
                (_ (length= right-value-children 1))
                (right-value (car right-value-children))
                (_ (string= (treesit-node-type right-value) "call_expression")))
      right-value))
   (t nil)))

(yas-snippet-go-mode-calling-function-p)
(defun yas-snippet-go-mode-calling-function-p ()
  (and
   (= ?\) (char-before))
   (save-excursion
     (back-to-indentation)
     (let ((parent (treesit-parent-until
                    (treesit-node-at (point))
                    (lambda (node)
                      (string-match-p "^\\(call_expression\\|block\\|short_var_declaration\\)$" (treesit-node-type node))))))
       (cond
        ((string= "block" (treesit-node-type parent))
         nil)
        ((string= "call_expression" (treesit-node-type parent))
         t)
        ((string= "short_var_declaration" (treesit-node-type parent))
         (yas-snippet-go-mode-get-call-expression-node parent))
        (t nil))))))

(defun yas-snippet-go-mode-remove-left-value (pos)
    (goto-char pos)
    (back-to-indentation)
    (when-let* ((parent (treesit-parent-until
                         (treesit-node-at (point))
                         (lambda (node)
                           (string= "short_var_declaration" (treesit-node-type node)))))
                (text (treesit-node-text (yas-snippet-go-mode-get-call-expression-node parent))))
      (delete-region (treesit-node-start parent) (treesit-node-end parent))
      (insert text)))

(defun yas-snippet-go-mode-get-function-response-at (pos)
  "Get calling function's response value at POS.
lsp-mode is required for this function."
  (save-excursion
    (goto-char pos)
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
    (cond
     ((or (string= type "pointer_type")
          (string= type "slice_type")
          (string= type "channel_type")
          (string= type "function_type")
          (string= name "error"))
      "nil")
     (t
      (cond
       ((or (string= "int" name)
            (string= "int64" name)
            (string= "int32" name))
        "0")
       ((string= "string" name) "\"\"")
       ((or (string= "byte" name)
            (string= "rune" name))
        "'0'")
       (t
        (format "%s\\{\\}" (treesit-node-text node))))))))

(defun yas-snippet-go-mode-get-response-name-snippet (names count)
  (setq count (- count 1))
  (string-join
   (mapcar
    (lambda (name) (format "${%d:%s}" (setq count (+ count 1)) name)) names) ", "))

(defun yas-snippet-go-mode-get-parent-function-return-values (pos)
  "Get parent calling function's return value of POS.

This function returns a plist, which contains
  :types       a list contains name of return types
  :names       a list contains name of return names.
               If name not specific, return the name of types.
  :type-nodes  a list contains return types (treesit node).
  :name-nodes  a list contains return names (treesit node).
                If name not specific, return the node of type."
  (when-let ((func-node (treesit-parent-until
                         (treesit-node-at pos)
                         (lambda (node)
                           (or (string= "function_declaration" (treesit-node-type node))
                               (string= "method_declaration" (treesit-node-type node))
                               (string= "func_literal" (treesit-node-type node))))))
             (return-values (if-let* ((children (nreverse (treesit-node-children func-node)))
                                      (_ (string= (treesit-node-type (car children)) "block")))
                                (when (string= (treesit-node-type (nth 2 children)) "parameter_list")
                                  (nth 1 children))
                              (car children))))
    (cond
     ((string= "parameter_list" (treesit-node-type return-values))
      (let ((parameters (seq-filter
                         (lambda (node)
                           (string= "parameter_declaration" (treesit-node-type node)))
                         (treesit-node-children return-values))))
        (list
         :types (mapcar 'yas-snippet--go-mode-get-type-by-treesit-node parameters)
         :names (mapcar 'yas-snippet--go-mode-get-name-by-treesit-node parameters)
         :type-nodes (mapcar 'yas-snippet--go-mode-get-type-node-by-treesit-node parameters)
         :name-nodes (mapcar 'yas-snippet--go-mode-get-name-node-by-treesit-node parameters))
        ))
     (t
      (if (string= "error" (treesit-node-text return-values))
          (list
           :types '("error")
           :names '("err")
           :type-nodes (list return-values)
           :name-nodes (list return-values))
        (list
         :types (list (treesit-node-text return-values))
         :names (list (treesit-node-text return-values))
         :type-nodes (list return-values)
         :name-nodes (list return-values)))))))

(defun yas-escape-text (text)
  "Escape TEXT for snippet."
  (when text
    (replace-regexp-in-string "[`\\$]" "\\\\\\&" text)))

(defun yas-snippet-go-mode-check-error-and-return (pos &optional inlinep)
  (goto-char pos)
  (yas-snippet-go-mode-remove-left-value (point))
  (when-let* ((plist (yas-snippet-go-mode-get-function-response-at (point)))
              (names (plist-get plist :names))
              (types (plist-get plist :types))
              (node-start (plist-get plist :node-start))
              (node-end (plist-get plist :node-end))
              (line (yas-escape-text (buffer-substring-no-properties node-start node-end))))
    (let ((err-idx (cl-position "error" types :test 'string=))
          (current-function (yas-snippet-go-mode-get-parent-function-return-values (point))))
      (if err-idx
          ;; if parent response contains error
          (let* ((return-type-nodes (plist-get current-function :type-nodes))
                 (return-name-indexes (mapcar (lambda (type)
                                                (cl-position type types :test 'string=))
                                              (plist-get current-function :types))))
            (let* ((i -1)
                   (return-names (mapcar (lambda (index)
                                           (setq i (+ 1 i))
                                           (or (and index (nth index names))
                                               (yas-snippet-go-mode-get-default-value (nth i return-type-nodes))))
                                         return-name-indexes)))
              (if (or (length= names 1) inlinep)
                  (yas-expand-snippet
                   (format "if %s := %s; $%d != nil {\n\treturn %s\n}"
                           (yas-snippet-go-mode-get-response-name-snippet names 1)
                           line
                           (+ 1 err-idx)
                           (yas-snippet-go-mode-get-response-name-snippet return-names (+ 1 (length names))))
                   node-start
                   node-end)
                (yas-expand-snippet
                 (format "%s := %s\nif $%d != nil {\n\treturn %s\n}"
                         (yas-snippet-go-mode-get-response-name-snippet names 1)
                         line
                         (+ 1 err-idx)
                         (yas-snippet-go-mode-get-response-name-snippet return-names (+ 1 (length names))))
                 node-start
                 node-end))))
        ;; if response doesn't have error, list up the parameters.
        (yas-expand-snippet
         (format "%s := %s" (yas-snippet-go-mode-get-response-name-snippet names 1) line)
         node-start
         node-end)))))

(defun yas-snippet-go-mode-check-error-and-panic (pos)
  (goto-char pos)
  (yas-snippet-go-mode-remove-left-value (point))
  (if-let ((func-resp (yas-snippet-go-mode-get-function-response-at (point))))
      (if-let ((err-idx (cl-position "error" (plist-get func-resp :types) :test 'string=))
               (names (plist-get func-resp :names))
               (line (yas-escape-text (buffer-substring-no-properties
                                       (plist-get func-resp :node-start)
                                       (plist-get func-resp :node-end)))))
          (if (length= names 1)
              (yas-expand-snippet
               (format "if ${1:err} := %s; $1 != nil {\n\tpanic($1)\n}"
                       line)
               (plist-get func-resp :node-start)
               (plist-get func-resp :node-end))
            (yas-expand-snippet
             (format "%s := %s\nif $%d != nil {\n\tpanic($%d)\n}"
                     (yas-snippet-go-mode-get-response-name-snippet names 1)
                     line
                     (+ 1 err-idx)
                     (+ 1 err-idx))
             (plist-get func-resp :node-start)
             (plist-get func-resp :node-end)))
        (yas-expand-snippet
         (format "%s := %s" (yas-snippet-go-mode-get-response-name-snippet names 1) line)
         (plist-get func-resp :node-start)
         (plist-get func-resp :node-end)))))

(defun yas-snippet-go-mode-check-error (pos &optional inlinep)
  (goto-char pos)
  (yas-snippet-go-mode-remove-left-value (point))
  (if-let ((func-resp (yas-snippet-go-mode-get-function-response-at (point))))
      (if-let ((err-idx (cl-position "error" (plist-get func-resp :types) :test 'string=))
               (names (plist-get func-resp :names))
               (line (yas-escape-text (buffer-substring-no-properties
                                       (plist-get func-resp :node-start)
                                       (plist-get func-resp :node-end)))))
          (if (or  (length= names 1) inlinep)
              (yas-expand-snippet
                   (format "if %s := %s; $%d != nil {\n\t$0\n}"
                           (yas-snippet-go-mode-get-response-name-snippet names 1)
                           line
                           (+ 1 err-idx)
                           (yas-snippet-go-mode-get-response-name-snippet names (+ 1 (length names))))
                   (plist-get func-resp :node-start)
                   (plist-get func-resp :node-end))
            (yas-expand-snippet
             (format "%s := %s\nif $%d != nil {\n\t$0\n}"
                     (yas-snippet-go-mode-get-response-name-snippet names 1)
                     line
                     (+ 1 err-idx)
                     (+ 1 err-idx))
             (plist-get func-resp :node-start)
             (plist-get func-resp :node-end)))
        (yas-expand-snippet
         (format "%s := %s"
                 (yas-snippet-go-mode-get-response-name-snippet
                  (plist-get func-resp :names) 1)
                 (yas-escape-text (buffer-substring-no-properties
                                   (plist-get func-resp :node-start)
                                   (plist-get func-resp :node-end))))
         (plist-get func-resp :node-start)
         (plist-get func-resp :node-end)))))

(defun yas-snippet-go-mode-ginkgo-check-error (pos &optional inlinep)
  (if-let ((func-resp (yas-snippet-go-mode-get-function-response-at pos)))
      (if-let ((err-idx (cl-position "error" (plist-get func-resp :types) :test 'string=))
               (names (plist-get func-resp :names))
               (line (yas-escape-text (buffer-substring-no-properties
                                       (plist-get func-resp :node-start)
                                       (plist-get func-resp :node-end)))))
          (cond
           ((length= names 1)
            (yas-expand-snippet
             (format "Expect(%s).To(Succeed())"
                     line)
             (plist-get func-resp :node-start)
             (plist-get func-resp :node-end)))
           (inlinep
            (yas-expand-snippet
             (format "Expect(%s).Error().To(Succeed())"
                     line)
             (plist-get func-resp :node-start)
             (plist-get func-resp :node-end)))
           (t
            (yas-expand-snippet
             (format "%s := %s\nExpect($%d).To(Succeed())\n$0"
                     (yas-snippet-go-mode-get-response-name-snippet names 1)
                     line
                     (+ 1 err-idx))
             (plist-get func-resp :node-start)
             (plist-get func-resp :node-end))))
        (if (length= names 1)
            (yas-expand-snippet (format "Expect(%s).To($0)" line))
          (yas-expand-snippet
           (format "%s := %s\nExpect($1).To($0)"
                   (yas-snippet-go-mode-get-response-name-snippet names 1) line)
           (plist-get func-resp :node-start)
           (plist-get func-resp :node-end))))))

(defun test ()
  (interactive)
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (node)
     (print (treesit-node-type node))
     nil)))
