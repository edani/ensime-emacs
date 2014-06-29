;;; ensime-server-runner.el --- Start and stop the server


(defun ensime-reload ()
  "Re-initialize the project with the current state of the config file.
Analyzer will be restarted. All source will be recompiled."
  (interactive)
  (ensime-assert-connected
   (let* ((conn (ensime-current-connection))
	  (current-conf (ensime-config conn))
	  (force-dir (plist-get current-conf :root-dir))
	  (config (ensime-config-load (ensime-config-find force-dir) force-dir)))

     (when (not (null config))
       (ensime-set-config conn config)
       (ensime-init-project conn config)))))

(defun ensime--maybe-start-server (buffer javahome flags jar env cache-dir request-port)
  "Return a new or existing inferior server process."
  (let (existing (comint-check-proc buffer))
    (if existing existing
      (ensime--start-server buffer javahome flags jar env cache-dir request-port))))

(defvar ensime-server-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun ensime--start-server (buffer javahome flags jar user-env cache-dir request-port)
  "Starts an ensime server in the given buffer.
   Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (comint-mode)
    (let* ((process-environment (append user-env process-environment))
	  (command (concat javahome "/bin/java"))
	  (tools (concat "-Xbootclasspath/a:" javahome "/lib/tools.jar"))
	  (args (-flatten (list tools
				"-classpath" jar "-Dscala.usejavacp=true"
				flags
				(concat "-Densime.cachedir=" (expand-file-name cache-dir))
				(concat "-Densime.requestport=" request-port)
				"org.ensime.server.Server"))))
      (set (make-local-variable 'comint-process-echoes) nil)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)
      (insert "Starting ENSIME: " command " " (mapconcat 'identity args " "))
      (comint-exec (current-buffer) buffer command nil args))
    (let ((proc (get-buffer-process (current-buffer))))
      (ensime-set-query-on-exit-flag proc)
      (run-hooks 'ensime-server-process-start-hook)
      proc)))


(defun ensime-shutdown()
  "Request that the current ENSIME server kill itself."
  (interactive)
  (ensime-quit-connection (ensime-current-connection)))

(defun ensime-configured-project-root ()
  "Return root path of the current project as defined in the
config file and stored in the current connection. Nil is returned
if there is no active connection, or if the project root was not
defined."
  (when (ensime-connected-p)
    (let ((config (ensime-config (ensime-current-connection))))
      (plist-get config :root-dir))))

(defun ensime-read-swank-port (portfile)
  "Read the Swank server port number from the `cache-dir',
   or nil if none was found."
  (when (file-exists-p portfile) 
    (save-excursion
      (with-temp-buffer
	(insert-file-contents portfile)
	(goto-char (point-min))
	(let ((port (read (current-buffer))))
	  (assert (integerp port))
	  port)))))

(defun ensime--retry-connect (server-proc config cache-dir attempts)
  (let* ((portfile (concat cache-dir "/port"))
	 (port (ensime-read-swank-port portfile)))
    (cond (ensime--abort-connection
	   (setq ensime--abort-connection nil)
	   (message "Aborted"))
	  ((>= 0 attempts)
	   (message "Ran out of connection attempts."))
	  ((eq (process-status server-proc) 'exit)
	   (message "Failed to connect: server process exited."))
	  (t
	   (if port
	       (ensime--connect server-proc config port)
	     (run-at-time "1 sec" nil
			  'ensime-timer-call 'ensime--retry-connect
			  server-proc config cache-dir (1- attempts)))))))

(defun ensime--connect (server-proc config port)
  ; non-nil if a connection was made, nil otherwise
  (let ((c (ensime-connect "127.0.0.1" port)))
    ;; It may take a few secs to get the source roots back from the
    ;; server, so we won't know immediately if currently visited
    ;; source is part of the new project. Make an educated guess for
    ;; the sake of UI snappiness (fast mode-line update).
    (when (and (ensime-source-file-p)
	       (plist-get config :root-dir)
	       (ensime-file-in-directory-p
		buffer-file-name
		(plist-get config :root-dir))
	       (not (ensime-connected-p)))
      (setq ensime-buffer-connection c))
    
    (ensime-set-config c config)
    
    (let ((ensime-dispatching-connection c))
      (ensime-eval-async
       '(swank:connection-info)
       (ensime-curry #'ensime-handle-connection-info c)))
    
    (ensime-set-server-process c server-proc)
    ;; As a convenience, we associate the client connection with the
    ;; server buffer. This assumes that there's only one client
    ;; connection per server. So far this is a safe assumption.
    (when-let (server-buf (process-buffer server-proc))
	      (with-current-buffer server-buf
		(setq ensime-buffer-connection c)))))
  
(defun ensime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.
   The default condition handler for timer functions (see
   `timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defvar ensime--abort-connection nil)

(defun ensime--abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (setq ensime-abort-connection 't))

(defun ensime-init-project (conn config)
  "Send configuration to the server process. Setup handler for
 project info that the server will return."
  (ensime-eval-async `(swank:init-project ,config)
		     (ensime-curry #'ensime-handle-project-info
				   conn)))


(defun ensime-handle-project-info (conn info)
  "Handle result of init-project rpc call. Install project information
computed on server into the local config structure."
  (let* ((config (ensime-config conn)))
    (setf config (plist-put config :project-name
			    (or
			     (plist-get config :project-name)
			     (plist-get info :project-name)
			     )))
    (setf config (plist-put config :source-roots
			    (plist-get info :source-roots)))
    (ensime-set-config conn config)
    (force-mode-line-update t)))

(provide 'ensime-server-runner)

;; Local Variables:
;; no-byte-compile: t
;; End:
