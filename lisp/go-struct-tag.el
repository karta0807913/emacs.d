(with-eval-after-load 'go-mode
  (require 'treesit)
  (require 'cl-generic)
  (require 's)
  (unless (treesit-parser-list)
    (treesit-parser-create 'go))

  (defgroup go-struct-tag nil
    "uses treesit to maintain the golang struct tags")

  (defcustom go-struct-tag-namer-functions
    (list
     :json 'go-struct-tag-json-namer-function
     :url 'go-struct-tag-snake-case-namer-function
     :form 'go-struct-tag-snake-case-namer-function)
    "tag and it's name generator
The key in the plist represents the go struct tag name you want to generate, and the returned value represents the tag value string

the the value-function's signature should be
(defun your-namer-function (info))

info is a plist has following keys
:parent        pure text of entire struct.
:parent-node   treesit node of parent.
:name          the name of field. nil if anonymous
:name-node     treesit node of :name. nil if anonymous. (type: field_identifier)
:type          name of the type. (type either type_identifier, struct_type, pointer_type, or slice_type)
:type-node     treesit node of :type. (type: field_identifier)
:tag-text      raw text of field tag.
:tag           parsed plist.
:tag-node      treesit node of :tag. And the (type: raw_string_literal)
Return Value:  string
"
    :type '(plist)
    :group 'go-struct-tag)

  (defcustom go-struct-tag-merger-functions
    (list
     :global '()
     :local '())
    "function to merge the old and new tags.
the \"local\" key will be call in every keys.
the \"global\" key is a list will be called after every \"local\" functions.

The function signature should be
(defun your-merger-function (info old-value new-value))
info:         refer to the doc string of to go-struct-tag-namer-functions
old-value:    plist of old tags.
new-value:    plist of new tags.
Return Value: plist of the new tags.
"
    :type '(plist
            :global (list (function-item))
            :local (plist))
    :group 'go-struct-tag)

  (defun go-struct-tag-add-gorm-struct-tag ()
    (interactive)
    (go-struct-tag-add-struct-tag :namer '(
                                           :json go-struct-tag-json-namer-function
                                           :url go-struct-tag-snake-case-namer-function
                                           :form go-struct-tag-snake-case-namer-function
                                           :gorm go-struct-tag-gorm-namer-function)))

  (cl-defun go-struct-tag-add-struct-tag (&optional &keys
                                                    (namer go-struct-tag-namer-functions)
                                                    (merger go-struct-tag-merger-functions))
    "add go tag to the end of each struct fields.
this function will use the custom variable go-struct-tag-namer-functions and go-struct-tag-merger-functions."
    (interactive)
    (when-let* ((root (go-struct-tag--treesit-get-struct-node (point)))
                (fields-info (go-struct-tag--treesit-get-struct-tag root root namer merger)))
      (go-struct-tag--modify-struct-tag fields-info)))

  (defun go-struct-tag--modify-struct-tag (fields-info)
    (when-let* ((field (car fields-info))
                (new-tag (go-struct-tag--plist-to-struct-tag (plist-get field :new-tag))))
      (let* ((marker (plist-get field :marker))
             (marker-point (marker-position marker))
             (tag-text (plist-get field :tag-text)))
        (unless (string-empty-p new-tag)
          (when tag-text
            (delete-region marker-point (+ marker-point (length tag-text))))
          (save-excursion
            (goto-char marker)
            (insert ? )
            (insert (format "`%s`" new-tag)))))
      (go-struct-tag--modify-struct-tag (cdr fields-info))))

  (cl-defun go-struct-tag--treesit-get-struct-tag (parent struct
                                                          &optional
                                                          (namer go-struct-tag-namer-functions)
                                                          (merger go-struct-tag-merger-functions))
    (go-struct-tag--treesit-get-field-list-tag
     parent
     (seq-find
      (lambda (node)
        (string= "field_declaration_list" (treesit-node-type node)))
      (treesit-node-children struct))
     namer
     merger))

  (defun go-struct-tag--treesit-get-field-list-tag (parent field-list namer merger)
    (let ((elems (remq nil
                       (mapcar
                        (lambda (node)
                          (when (string= "field_declaration" (treesit-node-type node))
                            (go-struct-tag--treesit-get-field-tag parent node namer merger)))
                        (treesit-node-children field-list))))
          (ans '()))
      (while (consp elems)
        (setq ans (append ans (car elems)))
        (setq elems (cdr elems)))
      ans))

  (defun go-struct-tag--treesit-get-field-tag (parent field namer merger)
    (let* ((children (treesit-node-children field))
           (inline-struct
            (seq-find (lambda (node)
                        (string= "struct_type" (treesit-node-type node)))
                      children))
           (pointer-type
            (seq-find (lambda (node)
                        (string= "pointer_type" (treesit-node-type node)))
                      children)))
      (when pointer-type
        (let ((children (treesit-node-children pointer-type)))
          (setq inline-struct (or inline-struct
                                  (seq-find
                                   (lambda (node)
                                     (string= "struct_type" (treesit-node-type node)))
                                   children)))))
      (append
       (when inline-struct
         (go-struct-tag--treesit-get-struct-tag field inline-struct namer merger))
       (list (go-struct-tag--treesit-convert-field-to-tag parent field namer merger)))))

  (defun go-struct-tag--treesit-convert-field-to-tag (parent node namer merger)
    (let* ((children (treesit-node-children node))
           (name-node
            (seq-find
             (lambda (node)
               (string= "field_identifier" (treesit-node-type node)))
             children))
           (type-node (seq-find
                       (lambda (node)
                         (let ((type (treesit-node-type node)))
                           (or (string= "type_identifier" type)
                               (string= "struct_type" type)
                               (string= "pointer_type" type)
                               (string= "slice_type" type)
                               (string= "qualified_type" type))))
                       children))
           (tag-node (seq-find
                      (lambda (node)
                        (string= "raw_string_literal" (treesit-node-type node)))
                      children)))
      (let* ((info (list
                    :parent (treesit-node-text parent)
                    :parent-node parent
                    :name (treesit-node-text name-node)
                    :name-node name-node
                    :type (treesit-node-text type-node)
                    :type-node type-node
                    :tag-text (and tag-node (treesit-node-text tag-node))
                    :tag (ignore-errors
                           (go-struct-tag-parse-struct-tag
                            (and tag-node (treesit-node-text tag-node))))
                    :tag-node tag-node))
             (new-tag (when-let* ((namer)
                                  (new-tag (go-struct-tag-namer-function info namer)))
                        new-tag)))
        `(,@info
          :marker ,(set-marker
                    (make-marker)
                    (or (treesit-node-start tag-node)
                        (treesit-node-end type-node))
                    (current-buffer))
          :new-tag ,(or (and merger (go-struct-tag-merger-function info new-tag merger))
                        new-tag
                        '())))))

  (defun go-struct-tag--treesit-get-struct-node (point)
    (treesit-parent-until
     (treesit-node-at point)
     (lambda (node)
       (string= "struct_type" (treesit-node-type node)))))

  (defun go-struct-tag-parse-struct-tag (string-literal)
    (if (= ?\" (string-to-char (or string-literal "")))
        (go-struct-tag--parse-struct-tag
         (string-to-list (json-parse-string string-literal)))
      (go-struct-tag--parse-struct-tag
       (cdr
        (butlast
         (string-to-list string-literal))))))

  ;; refer to https://cs.opensource.google/go/go/+/refs/tags/go1.22.2:src/reflect/type.go;l=923
  (defun go-struct-tag--parse-struct-tag (list-literal)
    (when list-literal
      (let ((original-list list-literal)
            (name '())
            (qvalue '())
            (char (car list-literal)))
        ;; removing leading space
        (while (and list-literal (= ?  (car list-literal)))
          (setq list-literal (cdr list-literal))
          (setq char (car list-literal)))
        ;; Scan to colon. A space, a quote or a control character is a syntax error.
        ;; Strictly speaking, control chars include the range [0x7f, 0x9f], not just
        ;; [0x00, 0x1f], but in practice, we ignore the multi-byte control characters
        ;; as it is simpler to inspect the tag's bytes than the tag's runes.
        (while (and
                list-literal
                (> char ? )
                (not (= char ?:))
                (not (= char ?\"))
                (not (= char #x7f)))
          (push char name)
          (setq list-literal (cdr list-literal))
          (setq char (car list-literal)))
        (when (or
               (not list-literal)
               (not (cdr list-literal))
               (not name)
               (not (= char ?:))
               (not (= (car (cdr list-literal)) ?\")))
          (throw
           'parse-error
           (format
            "%s is not not compatible with reflect.StructTag.Get"
            (apply 'string original-list))))
        (setq list-literal (cdr (cdr list-literal)))
        (setq char (car list-literal))
        (while
            (and
             list-literal
             (not (= char ?\")))
          (when (= char ?\\)
            (push char qvalue)
            (setq list-literal (cdr list-literal))
            (setq char (car list-literal)))
          (push char qvalue)
          (setq list-literal (cdr list-literal))
          (setq char (car list-literal)))
        `(
          ,(intern (apply 'string (cons ?: (nreverse name))))
          ,(apply 'string (nreverse qvalue))
          ,@(go-struct-tag--parse-struct-tag (cdr list-literal))))))

  (defun go-struct-tag--plist-to-struct-tag (tags)
    (let ((keys (go-struct-tag-get-plist-keys tags))
          (ans ""))
      (while-let ((key (car keys)))
        (when (plist-get tags key)
          (setq ans (concat ans
                            (format " %s:%s"
                                    (substring (symbol-name key) 1)
                                    (json-encode (plist-get tags key))))))

        (setq keys (cdr keys)))
      (if (length> ans 0)
          (substring ans 1)
        ans)))

  (defun go-struct-tag--treesit-reduce (list1 list2)
    (while-let ((key (car list2)))
      (unless (member key list1)
        (push key list1))
      (setq list2 (cdr list2)))
    list1)

  (defun go-struct-tag-get-plist-keys (plist)
    (let ((ans '()))
      (while-let ((key (car plist)))
        (unless (member key ans)
          (push key ans))
        (setq plist (cdr (cdr plist))))
      (nreverse ans)))

  (cl-defun go-struct-tag-merger-function (info new-tags &optional
                                                (merger go-struct-tag-merger-functions))
    (let* ((old-tags (plist-get info :tag))
           (local-functions (plist-get merger :local))
           (global-functions (plist-get merger :global))
           (keys (go-struct-tag--treesit-reduce
                  (go-struct-tag-get-plist-keys old-tags)
                  (go-struct-tag-get-plist-keys new-tags))))
      (setq new-tags
            (go-struct-tag--treesit-merger-function info keys old-tags new-tags local-functions))
      (while-let ((global-merger (car global-functions)))
        (setq new-tags (funcall merger info old-tags new-tags))
        (setq global-functions (cdr global-functions))))
    new-tags)

  (defun go-struct-tag--treesit-merger-function (info keys old-tags new-tags &optional local-functions)
    (when-let ((key (car keys)))
      (let ((old-value (plist-get old-tags key))
            (new-value (plist-get new-tags key)))
        (if-let ((merger (plist-get local-functions key)))
            (setq old-tags (plist-put old-tags key (funcall merger info old-value new-value)))
          (unless old-value
            (setq old-tags (plist-put old-tags key new-value))))
        (setq old-tags
              (go-struct-tag--treesit-merger-function info (cdr keys) old-tags new-tags local-functions))))
    old-tags)

  (cl-defun go-struct-tag-namer-function (info &optional
                                               (namer go-struct-tag-namer-functions))
    (go-struct-tag--treesit-namer-function info namer))

  (defun go-struct-tag--treesit-namer-function (info functions)
    (when-let ((tag (nth 0 functions))
               (func (nth 1 functions)))
      `(,tag
        ,(let* ((name (funcall func info))
                (_ (or (string-or-null-p name)
                       (throw 'namer-function-error
                              (format "namer function %s is not returning string"
                                      namer)))))
           name)
        ,@(go-struct-tag--treesit-namer-function
           info (cdr (cdr functions))))))

  (defun go-struct-tag-snake-case-namer-function (info)
    "return snake-case name."
    (when-let* ((name (plist-get info :name))
                (_ (not (string-empty-p name))))
      (s-snake-case name)))

  (defun go-struct-tag-gorm-namer-function (info)
    "returns snake-case name.
If type name is \"id\", it will add tag \"primaryKey\".
If type is not pointer_type. it will add tag \"not null\"."
    (when-let* ((name (go-struct-tag-snake-case-namer-function info))
                (type-node (plist-get info :type-node))
                (tag-list `(,(format "column:%s" name))))
      (when (string= name "id")
        (push "primaryKey" tag-list)
        (push "autoIncrement" tag-list))
      (unless (string= "pointer_type" (treesit-node-type type-node))
        (push "not null" tag-list))
      (string-join (nreverse tag-list) ";")))

  (defun go-struct-tag-json-namer-function (info)
    "returns snake-case name. if variable is \"pointer_type\", set omitempty."
    (when-let* ((name (go-struct-tag-snake-case-namer-function info))
                (type-node (plist-get info :type-node))
                (tag-list `(,name)))
      (when (string= "pointer_type" (treesit-node-type type-node))
        (push "omitempty" tag-list))
      (string-join (nreverse tag-list) ","))))

(provide 'go-struct-tag)
