(defun lsp-java-format-hook ()
  ;; (lsp-java-add-import)
  (lsp-java-organize-imports)
  (lsp-format-buffer)
  )

(defun lsp-java-init ()
  (add-hook 'before-save-hook #'lsp-java-format-hook t t)
  (general-create-definer java-leader-def
    :prefix ","
    :states '(normal emacs)
    :keymaps 'java-mode-map)
  (java-leader-def
   "op" (lambda ()
          (interactive)
          (message "start java building")
          (lsp-java-build-project)))
  (flycheck-mode +1)
  (lsp-java)
  (lsp))

;; mvn archetype:generate -DgroupId=com.mycompany.app -DartifactId=my-app -DarchetypeArtifactId=maven-archetype-quickstart -DarchetypeVersion=1.4 -DinteractiveMode=false
(defun lsp-java-create-project ()
  (interactive)
  "create java project by using mvnw"
  (let (
        (mvnw (executable-find "mvnw"))
        (project-location nil)
        (project-name "my-app")
        (group "com.example.app")
        (template "maven-archetype-quickstart")
        (version "1.0")
        (mvnw-buffer-name "*mvnw-log*")
        )
    (if (not mvnw)
        (user-error "mvnw executable not found"))
    (setq project-location (read-directory-name "Project Location: "))
    (setq project-name (read-string (format "Project Name (default: %s): " project-name) nil nil project-name))
    (if (file-exists-p (concat (file-name-as-directory project-location) project-name))
        (user-error (format "Path %s exists" (concat (file-name-as-directory project-location) project-name))))
    (setq group (read-string (format "Project Group ID (default com.example.%s): " group) nil nil group))
    (setq template (read-string (format "Archetype (default %s): " template) nil nil template))
    (setq version (read-string (format "Version (default %s): " version) nil nil version))
    (set-process-filter (start-process "mvnw" mvnw-buffer-name mvnw
                                       "archetype:generate"
                                       (format "-DgroupId=%s" group)
                                       (format "-DartifactId=%s" project-name)
                                       (format "-DarchetypeArtifactId=%s" template)
                                       (format "-DarchetypeVersion=%s" version)
                                       "-DinteractiveMode=false") 'comint-output-filter)
    (display-buffer mvnw-buffer-name)))


(add-hook 'java-mode-hook '(lsp-java-init))
(provide 'init-java)
