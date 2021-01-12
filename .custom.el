(setq line-number-mode t)
(setq scroll-step            1
      scroll-conservatively  10000)

(setq create-lockfiles nil) ;; don't create lock files
(setq visible-bell nil)
(load-theme 'moe-dark)
;; (load-theme 'ample-flat) ;; for putty

(setq neo-window-width 35)
(set-language-environment "UTF-8")
;; (setq counsel-etags-debug t)
(setq lsp-latex-forward-search-executable "zathura")
(setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))

(defun eshell/clear ()
  "clear eshell buffer"
  (interactive)
  (let ((eshell-buffer (get-buffer "*eshell*")))
    (if (equal eshell-buffer nil)
        (print '"no eshell exist")
      (with-current-buffer eshell-buffer
        (let ((eshell-buffer-maximum-lines 0))
          (eshell-truncate-buffer))))))

(defun copy-region-to-xserver ()
  "copy current region to xcclb"
  (interactive)
  (let ((xclip (executable-find "xclip")))
    (if (= (length xclip) 0)
        (message "can't find xclip, copy failed")
      (let (
            (process (start-process "xclip" "xclip" xclip "-selection" "clipboard"))
            (start-point (if (use-region-p)
                             (region-beginning)
                           (point-min)))
            (end-point (if (use-region-p)
                           (region-end)
                         (point-max)))
            )
        (process-send-region process start-point end-point)
        ;; (process-send-string process
        ;;                      (buffer-substring start-point end-point))
        ;; (process-send-eof process)
        (deactivate-mark)
        (message (format "copy start at %d, end at %d" start-point end-point))
        ))))

(require 'request)
(defun markdown-live-preview-buffer ()
  (interactive)
  (let ((data (buffer-string)))
    (request
     "https://api.github.com/markdown"
     ;; "http://httpbin.org/post"
     :type "POST"
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode `(("text" . ,data)))
     :parser (lambda () (buffer-string))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (with-current-buffer (get-buffer-create "*preview*")
                   (erase-buffer)
                   (insert data)
                   (insert "<style> body { font-family: Helvetica, arial, sans-serif; font-size: 14px; line-height: 1.6; padding-top: 10px; padding-bottom: 10px; background-color: white; padding: 30px; } body > *:first-child { margin-top: 0 !important; } body > *:last-child { margin-bottom: 0 !important; } a { color: #4183C4; } a.absent { color: #cc0000; } a.anchor { display: block; padding-left: 30px; margin-left: -30px; cursor: pointer; position: absolute; top: 0; left: 0; bottom: 0; } h1, h2, h3, h4, h5, h6 { margin: 20px 0 10px; padding: 0; font-weight: bold; -webkit-font-smoothing: antialiased; cursor: text; position: relative; } h1:hover a.anchor, h2:hover a.anchor, h3:hover a.anchor, h4:hover a.anchor, h5:hover a.anchor, h6:hover a.anchor { background: no-repeat 10px center; text-decoration: none; } h1 tt, h1 code { font-size: inherit; } h2 tt, h2 code { font-size: inherit; } h3 tt, h3 code { font-size: inherit; } h4 tt, h4 code { font-size: inherit; } h5 tt, h5 code { font-size: inherit; } h6 tt, h6 code { font-size: inherit; } h1 { font-size: 28px; color: black; } h2 { font-size: 24px; border-bottom: 1px solid #cccccc; color: black; } h3 { font-size: 18px; } h4 { font-size: 16px; } h5 { font-size: 14px; } h6 { color: #777777; font-size: 14px; } p, blockquote, ul, ol, dl, li, table, pre { margin: 15px 0; } hr { background: transparent repeat-x 0 0; border: 0 none; color: #cccccc; height: 4px; padding: 0; } body > h2:first-child { margin-top: 0; padding-top: 0; } body > h1:first-child { margin-top: 0; padding-top: 0; } body > h1:first-child + h2 { margin-top: 0; padding-top: 0; } body > h3:first-child, body > h4:first-child, body > h5:first-child, body > h6:first-child { margin-top: 0; padding-top: 0; } a:first-child h1, a:first-child h2, a:first-child h3, a:first-child h4, a:first-child h5, a:first-child h6 { margin-top: 0; padding-top: 0; } h1 p, h2 p, h3 p, h4 p, h5 p, h6 p { margin-top: 0; } li p.first { display: inline-block; } ul, ol { padding-left: 30px; } ul :first-child, ol :first-child { margin-top: 0; } ul :last-child, ol :last-child { margin-bottom: 0; } dl { padding: 0; } dl dt { font-size: 14px; font-weight: bold; font-style: italic; padding: 0; margin: 15px 0 5px; } dl dt:first-child { padding: 0; } dl dt > :first-child { margin-top: 0; } dl dt > :last-child { margin-bottom: 0; } dl dd { margin: 0 0 15px; padding: 0 15px; } dl dd > :first-child { margin-top: 0; } dl dd > :last-child { margin-bottom: 0; } blockquote { border-left: 4px solid #dddddd; padding: 0 15px; color: #777777; } blockquote > :first-child { margin-top: 0; } blockquote > :last-child { margin-bottom: 0; } table { padding: 0; } table tr { border-top: 1px solid #cccccc; background-color: white; margin: 0; padding: 0; } table tr:nth-child(2n) { background-color: #f8f8f8; } table tr th { font-weight: bold; border: 1px solid #cccccc; text-align: left; margin: 0; padding: 6px 13px; } table tr td { border: 1px solid #cccccc; text-align: left; margin: 0; padding: 6px 13px; } table tr th :first-child, table tr td :first-child { margin-top: 0; } table tr th :last-child, table tr td :last-child { margin-bottom: 0; } img { max-width: 100%; } span.frame { display: block; overflow: hidden; } span.frame > span { border: 1px solid #dddddd; display: block; float: left; overflow: hidden; margin: 13px 0 0; padding: 7px; width: auto; } span.frame span img { display: block; float: left; } span.frame span span { clear: both; color: #333333; display: block; padding: 5px 0 0; } span.align-center { display: block; overflow: hidden; clear: both; } span.align-center > span { display: block; overflow: hidden; margin: 13px auto 0; text-align: center; } span.align-center span img { margin: 0 auto; text-align: center; } span.align-right { display: block; overflow: hidden; clear: both; } span.align-right > span { display: block; overflow: hidden; margin: 13px 0 0; text-align: right; } span.align-right span img { margin: 0; text-align: right; } span.float-left { display: block; margin-right: 13px; overflow: hidden; float: left; } span.float-left span { margin: 13px 0 0; } span.float-right { display: block; margin-left: 13px; overflow: hidden; float: right; } span.float-right > span { display: block; overflow: hidden; margin: 13px auto 0; text-align: right; } code, tt { margin: 0 2px; padding: 0 5px; white-space: nowrap; border: 1px solid #eaeaea; background-color: #f8f8f8; border-radius: 3px; } pre code { margin: 0; padding: 0; white-space: pre; border: none; background: transparent; } .highlight pre { background-color: #f8f8f8; border: 1px solid #cccccc; font-size: 13px; line-height: 19px; overflow: auto; padding: 6px 10px; border-radius: 3px; } pre { background-color: #f8f8f8; border: 1px solid #cccccc; font-size: 13px; line-height: 19px; overflow: auto; padding: 6px 10px; border-radius: 3px; } pre code, pre tt { background-color: transparent; border: none; } </style>")
                   (html-mode)
                   (if browse-url-temp-file-name
                     (message "C"))
                   (browse-url-of-buffer (current-buffer))
                   ;; (eww-display-html 'utf-8 nil
                   ;;                   (libxml-parse-html-region (point-min) (point-max))
                   ;;                   (point) (current-buffer))
                   )))
     :error (cl-function
             (lambda (&rest args &key error-thrown &allow-other-keys)
               (message "Got error: %S" error-thrown))))
    )
  )
(defun latex-beamer-formula-frame ()
  (interactive)
  (let ((frametitle (read-from-minibuffer "Frame Title:")))
    (insert (format "%%---------------------------------------------------------
\\begin{frame}
  \\frametitle{%s}
  \\begin{columns}[T]
    \\begin{column}{.5\\textwidth}
      \\begin{itemize}
      \\end{itemize}
    \\end{column}
    \\begin{column}{.5\\textwidth}
      \\begin{itemize}
      \\end{itemize}
    \\end{column}
  \\end{columns}
  \\vfill
  \\begin{align*} \\end{align*}
\\end{frame}
%%---------------------------------------------------------" frametitle))))

;;
;; (message test_buffer)
;; (message test_process)
;; (buffer-substring 1, 10)
;; (print test_process)
;; (print test_buffer)
;; (with-current-buffer test_buffer
;;  ;; (buffer-name)
;;   ;; (buffer-file-name)
;;   ;; (buffer-substring 1 (point-max))
;;   (buffer-string)
;;   ;; (erase-buffer)
;;   )
;; (buffer-file-name)
;; (buffer-substring 1 10)
;; (buffer-string)
;; (setq test_process (open-network-stream "test stream" test_buffer "127.0.0.1" 9000 "plain"))
;; (setq test_buffer (wsc--create-data-buffer))
;; (setq test_buffer (get-buffer-create " wsc "))
;; (setq test_process (open-network-stream "test stream" test_buffer "127.0.0.1" 9229 "plain"))
;; (setq log_buffer (get-buffer-create " log "))
;; (setq http_get_header (concat "GET / HTTP/1.1\r\n"
;;                                 "HOST 127.0.0.1:9000 \r\n"
;;                                 "\r\n"))
;; (defvar wsc--handshake-template (concat
;;                                "GET %s HTTP/1.1\n"
;; 				               "Host: %s\n"
;; 				               "Upgrade: websocket\n"
;; 				               "Connection: Upgrade\n"
;; 				               "Sec-WebSocket-Key: %s\n"
;; 				               "Sec-WebSocket-Version: %s\n\n"))

;; (defvar _wsc--handshake-template (concat
;;                                "GET %s HTTP/1.1\n"
;; 				               "Host: %s\n"
;; 				               "Upgrade: websocket\n"
;; 				               "Connection: Upgrade\n"
;; 				               "Sec-WebSocket-Key: %s\n"
				               ;; "Sec-WebSocket-Version: %s\n\n"))
;; (process-send-string test_process http_get_header)
;; (process-send-string test_process (format wsc--handshake-template "/e08ad0be-32a2-4d57-a6bd-ebde200b2cd8" "127.0.0.1:9229" "AAAATFUHS" "13" ))
;; (with-current-buffer test_buffer
;;  ;; (buffer-name)
;;   ;; (buffer-file-name)
;;   ;; (buffer-substring 1 (point-max))
;;   (buffer-string)
;;   ;; (erase-buffer)
;;   )
;; (with-current-buffer log_buffer
;;   (buffer-string)
;;   )
;; (with-current-buffer log_buffer
;;   (erase-buffer)
;;   )
;; (with-current-buffer test_buffer
;;   (erase-buffer)
;;   )
;; (delete-process test_process)

;; (defun wsc--open-handshake (connection)
;;   "Open a handshake for CONNECTION."
;;   (let* ((process (wsc-connection-process connection))
;; 	 (url (wsc-connection-url connection))
;; 	 (absolute-path (if (string-empty-p (url-filename url))
;; 			    "/"
;; 			  (url-filename url)))
;; 	 (host-and-port (if (url-port-if-non-default url)
;; 			    (format "%s:%s" (url-host url) (url-port url))
;; 			  (url-host url))))
;;     (process-send-string process (format _wsc--handshake-template
;; 					 absolute-path
;; 					 host-and-port
;; 					 (wsc-connection-key connection)
					 ;; wsc-protocol-version))))
;; (defun wsc--make-process (url)
;;   "Open a TCP connection to URL, and return the process object."
;;   (let* ((use-tls (string= (url-type url) "wss"))
;; 	 (type (if use-tls 'tls 'plain))
;; 	 (host (url-host url))
;; 	 (port (if (zerop (url-port url))
;; 		   (if use-tls 443 80)
;; 		 (url-port url)))
;; 	 (name "wsc process")
;; 	 (buf (wsc--create-data-buffer)))
;;     (with-current-buffer (get-buffer-create " log ")
;;       (goto-char (point-max))
;;       (insert (format "%s:%s\n%s\n\n" host port type)))
;;     (open-network-stream "test stream" test_buffer "127.0.0.1" 9229 "plain")))

;; (defun wsc--open-handshake (connection)
;;   "Open a handshake for CONNECTION."
;;   (let* ((process (wsc-connection-process connection))
;; 	 (url (wsc-connection-url connection))
;; 	 (absolute-path (if (string-empty-p (url-filename url))
;; 			    "/"
;; 			  (url-filename url)))
;; 	 (host-and-port (if (url-port-if-non-default url)
;; 			    (format "%s:%s" (url-host url) (url-port url))
;; 			  (url-host url))))
;;     (process-send-string process (format wsc--handshake-template "/b65e221d-a310-4d11-a868-554854d6e1b3" "127.0.0.1:9229" "AAAATFUHS" "13" ))))

;; (with-current-buffer test_buffer
;;   (goto-char (point-max))
;;   (insert wsc--handshake-template))

;; (wsc--make-process '(:type "ws" :user nil :password nil :host "127.0.0.1" :portspec 9229 :filename "/b65e221d-a310-4d11-a868-554854d6e1b3/6f693b9e-3262-49fc-9a97-e3e086715ad8" :target nil :attributes nil :fullness t :silent nil :use-cookies t :asynchronous t) test_buffer)
