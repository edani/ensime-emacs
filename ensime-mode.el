;;; ensime-mode.el --- ensime mode

;;;;; ensime-mode
(defun ensime-scala-mode-hook ()
  "Conveniance hook function that just starts ensime-mode."
  (ensime-mode 1))

(defvar ensime-source-buffer-saved-hook nil
  "Hook called whenever an ensime source buffer is saved.")

(defvar ensime-source-buffer-loaded-hook nil
  "Hook called whenever an ensime source buffer is loaded.")

(defun ensime-run-after-save-hooks ()
  "Things to run whenever a source buffer is saved."
  (when (and (ensime-connected-p) (ensime-analyzer-ready))
    (condition-case err-info
        (run-hooks 'ensime-source-buffer-saved-hook)
      (error
       (message
        "Error running ensime-source-buffer-saved-hook: %s"
        err-info)))))

(defun ensime-run-find-file-hooks ()
  "Things to run whenever a source buffer is opened."
  (when (and (ensime-connected-p) (ensime-analyzer-ready))
    (condition-case err-info
        (run-hooks 'ensime-source-buffer-loaded-hook)
      (error
       (message
        "Error running ensime-source-buffer-loaded-hook: %s"
        err-info)))))

(defun ensime-save-buffer-no-hooks ()
  "Just save the buffer per usual, don't type-check!"
  (let ((after-save-hook nil)
        (before-save-hook nil))
    (save-buffer)))

(defun ensime-delete-buffer-and-file ()
  "Kill the current buffer and delete the corresponding file!"
  (interactive)
  (ensime-assert-buffer-saved-interactive
   (let ((f buffer-file-name))
     (ensime-rpc-remove-file f)
     (delete-file f)
     (kill-buffer nil)
     )))


(defvar ensime-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))

      (define-key prefix-map (kbd "C-v i") 'ensime-inspect-type-at-point)
      (define-key prefix-map (kbd "C-v 5 i")
	'ensime-inspect-type-at-point-other-frame)
      (define-key prefix-map (kbd "C-v p") 'ensime-inspect-package-at-point)
      (define-key prefix-map (kbd "C-v o") 'ensime-inspect-project-package)
      (define-key prefix-map (kbd "C-v r") 'ensime-show-uses-of-symbol-at-point)
      (define-key prefix-map (kbd "C-v s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-v z") 'ensime-inf-switch)
      (define-key prefix-map (kbd "C-v f") 'ensime-format-source)
      (define-key prefix-map (kbd "C-v u") 'ensime-undo-peek)
      (define-key prefix-map (kbd "C-v v") 'ensime-search)
      (define-key prefix-map (kbd "C-v x") 'ensime-scalex)
      (define-key prefix-map (kbd "C-v d") 'ensime-show-doc-for-symbol-at-point)
      (define-key prefix-map (kbd "C-v t") 'ensime-print-type-at-point)
      (define-key prefix-map (kbd "C-v e") 'ensime-print-errors-at-point)
      (define-key prefix-map (kbd "C-v .") 'ensime-expand-selection-command)

      (define-key prefix-map (kbd "C-v C-r") 'ensime-inf-eval-region)
      (define-key prefix-map (kbd "C-v b") 'ensime-inf-eval-buffer)
      (define-key prefix-map (kbd "C-v l") 'ensime-inf-load-file)

      (define-key prefix-map (kbd "C-c c") 'ensime-typecheck-current-file)
      (define-key prefix-map (kbd "C-c a") 'ensime-typecheck-all)
      (define-key prefix-map (kbd "C-c r") 'ensime-reload-open-files)
      (define-key prefix-map (kbd "C-c e") 'ensime-show-all-errors-and-warnings)

      (define-key prefix-map (kbd "C-t t") 'ensime-goto-test)
      (define-key prefix-map (kbd "C-t i") 'ensime-goto-impl)

      (define-key prefix-map (kbd "C-d d") 'ensime-db-start)
      (define-key prefix-map (kbd "C-d b") 'ensime-db-set-break)
      (define-key prefix-map (kbd "C-d u") 'ensime-db-clear-break)
      (define-key prefix-map (kbd "C-d s") 'ensime-db-step)
      (define-key prefix-map (kbd "C-d o") 'ensime-db-step-out)
      (define-key prefix-map (kbd "C-d n") 'ensime-db-next)
      (define-key prefix-map (kbd "C-d r") 'ensime-db-run)
      (define-key prefix-map (kbd "C-d c") 'ensime-db-continue)
      (define-key prefix-map (kbd "C-d q") 'ensime-db-quit)
      (define-key prefix-map (kbd "C-d i") 'ensime-db-inspect-value-at-point)
      (define-key prefix-map (kbd "C-d t") 'ensime-db-backtrace)
      (define-key prefix-map (kbd "C-d a") 'ensime-db-clear-all-breaks)

      (define-key prefix-map (kbd "C-b s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-b S") 'ensime-stacktrace-switch)
      (define-key prefix-map (kbd "C-b c") 'ensime-sbt-do-compile)
      (define-key prefix-map (kbd "C-b n") 'ensime-sbt-do-clean)
      (define-key prefix-map (kbd "C-b o") 'ensime-sbt-do-test-only)
      (define-key prefix-map (kbd "C-b p") 'ensime-sbt-do-package)
      (define-key prefix-map (kbd "C-b r") 'ensime-sbt-do-run)
      (define-key prefix-map (kbd "C-b T") 'ensime-sbt-do-test)
      (define-key prefix-map (kbd "C-b t") 'ensime-sbt-do-test-quick)

      (define-key prefix-map (kbd "C-d u") 'ensime-db-clear-break)
      (define-key prefix-map (kbd "C-d s") 'ensime-db-step)
      (define-key prefix-map (kbd "C-d n") 'ensime-db-next)
      (define-key prefix-map (kbd "C-d r") 'ensime-db-run)
      (define-key prefix-map (kbd "C-d c") 'ensime-db-continue)
      (define-key prefix-map (kbd "C-d q") 'ensime-db-quit)
      (define-key prefix-map (kbd "C-d l") 'ensime-db-list-locals)

      (define-key prefix-map (kbd "C-r r") 'ensime-refactor-rename)
      (define-key prefix-map (kbd "C-r o") 'ensime-refactor-organize-imports)
      (define-key prefix-map (kbd "C-r l") 'ensime-refactor-extract-local)
      (define-key prefix-map (kbd "C-r m") 'ensime-refactor-extract-method)
      (define-key prefix-map (kbd "C-r i") 'ensime-refactor-inline-local)
      (define-key prefix-map (kbd "C-r t") 'ensime-import-type-at-point)

      (define-key map ensime-mode-key-prefix prefix-map)

      ;; Prefix-less shortcuts bindings...
      (define-key map (kbd "M-.") 'ensime-edit-definition)
      (define-key map (kbd "M-,") 'ensime-pop-find-definition-stack)

      (define-key map (kbd "M-n") 'ensime-forward-note)
      (define-key map (kbd "M-p") 'ensime-backward-note)

      (define-key map [C-down-mouse-1] 'ignore)
      (define-key map [C-up-mouse-1] 'ignore)
      (define-key map [C-down-mouse-3] 'ignore)
      (define-key map [C-up-mouse-3] 'ignore)
      (define-key map [C-mouse-1] 'ensime-control-mouse-1-single-click)
      (define-key map [C-mouse-3] 'ensime-control-mouse-3-single-click)
      )

    map)
  "Keymap for ENSIME mode."
  )

(easy-menu-define ensime-mode-menu ensime-mode-map
  "Menu for ENSIME mode"
  '("ENSIME"
    ("Test")

    ("Source"
     ["Format source" ensime-format-source]
     ["Find all references" ensime-show-uses-of-symbol-at-point]
     ["Inspect type" ensime-inspect-type-at-point]
     ["Inspect type in another frame" ensime-inspect-type-at-point-other-frame]
     ["Inspect enclosing package" ensime-inspect-package-at-point]
     ["Inspect project package" ensime-inspect-project-package]
     ["Undo source change" ensime-undo-peek])

    ("Typecheck"
     ["Typecheck file" ensime-typecheck-current-file]
     ["Typecheck project" ensime-typecheck-all]
     ["Reload typechecker" ensime-reload-open-files]
     ["Show all errors and warnings" ensime-show-all-errors-and-warnings])

    ("Refactor"
     ["Organize imports" ensime-refactor-organize-imports]
     ["Import type at point" ensime-import-type-at-point]
     ["Rename" ensime-refactor-rename]
     ["Extract local val" ensime-refactor-extract-local]
     ["Extract method" ensime-refactor-extract-method]
     ["Inline local val" ensime-refactor-inline-local])

    ("Navigation"
     ["Lookup definition" ensime-edit-definition]
     ["Lookup definition in other window" ensime-edit-definition-other-window]
     ["Lookup definition in other frame" ensime-edit-definition-other-frame]
     ["Go to test class" ensime-goto-test]
     ["Go to implementation class" ensime-goto-impl]
     ["Pop definition stack" ensime-pop-find-definition-stack]
     ["Backward compilation note" ensime-backward-note]
     ["Forward compilation note" ensime-forward-note]
     ["Expand selection" ensime-expand-selection-command]
     ["Search" ensime-search]
     ["Scalex-Search" ensime-scalex])

    ("Documentation"
     ["Browse documentation of symbol" ensime-show-doc-for-symbol-at-point])

    ("SBT"
     ["Start or switch to" ensime-sbt-switch]
     ["Compile" ensime-sbt-do-compile]
     ["Clean" ensime-sbt-do-clean]
     ["Test" ensime-sbt-do-test]
     ["Test Quick" ensime-sbt-do-test-quick]
     ["Test current class" ensime-sbt-do-test-only]
     ["Run" ensime-sbt-do-run]
     ["Package" ensime-sbt-do-package])

    ("Debugger"
     ["Start" ensime-db-start]
     ["Set break point" ensime-db-set-break]
     ["Clear breakpoint" ensime-db-clear-break]
     ["Clear all breakpoints" ensime-db-clear-all-breaks]
     ["Step" ensime-db-step]
     ["Next" ensime-db-next]
     ["Run" ensime-db-run]
     ["Continue" ensime-db-continue]
     ["Quit" ensime-db-quit]
     ["Show Backtrace" ensime-db-backtrace]
     ["Inspect value at point" ensime-db-inspect-value-at-point]
     )

    "---"
    ["Go to SBT console" ensime-sbt-switch]
    ["Go to stacktrace buffer" ensime-stacktrace-switch]
    ["Go to Scala REPL" ensime-inf-switch]
    ["Shutdown ENSIME server" ensime-shutdown]
    ))

(define-minor-mode ensime-mode
  "ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode).
\\{ensime-mode-map}"
  nil
  nil
  ensime-mode-map

  (if ensime-mode
      (progn

	(pcase ensime-completion-style
	  (`company
	   (ensime-company-enable))
	  (`auto-complete
	   (ensime-ac-enable))
	  (_ t))

	;; Always add the cap function. Some users may prefer it,
	;; and it may be useful in other completion modes (helm?).
	(add-hook 'completion-at-point-functions
		  'ensime-completion-at-point-function nil t)

        (easy-menu-add ensime-mode-menu ensime-mode-map)

        (add-hook 'after-save-hook 'ensime-run-after-save-hooks nil t)

	(add-hook 'find-file-hook 'ensime-run-find-file-hooks nil t)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-typecheck-current-file)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-typecheck-current-file)

        (add-hook 'after-change-functions
                  'ensime-after-change-function nil t)

        (ensime-idle-typecheck-set-timer)

        (when ensime-tooltip-hints
          (add-hook 'tooltip-functions 'ensime-tooltip-handler)
          (make-local-variable 'track-mouse)
          (setq track-mouse t)
          (make-local-variable 'tooltip-delay)
          (setq tooltip-delay 1.0)
          (define-key ensime-mode-map [mouse-movement] 'ensime-mouse-motion))

        (ensime-refresh-all-note-overlays))
    (progn
      (ensime-ac-disable)
      (remove-hook 'after-save-hook 'ensime-run-after-save-hooks t)

      (remove-hook 'find-file-hook 'ensime-run-find-file-hooks t)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-typecheck-current-file)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-typecheck-current-file)

      (remove-hook 'after-change-functions
                   'ensime-after-change-function t)

      (remove-hook 'tooltip-functions 'ensime-tooltip-handler)
      (make-local-variable 'track-mouse)
      (setq track-mouse nil))))

;;;;;; Mouse handlers

(defun ensime-control-mouse-1-single-click (event)
  "Command handler for control+clicks of mouse button 1.
   If control is held, jump to definition of symbol under
   point."
  (interactive "e")
  (mouse-set-point event)
  (ensime-edit-definition))

(defun ensime-control-mouse-3-single-click (event)
  "Command handler for double clicks of mouse button 1.
   If the user clicks on a package declaration or import,
   inspect that package. Otherwise, try to inspect the type
   of the thing at point."
  (interactive "e")
  (ensime-inspect-type-at-point))


(defun ensime-mouse-motion (event)
  "Command handler for mouse movement events in `ensime-mode-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))


;;;;;; Tooltips


(defun ensime-tooltip-show-message (msg)
  "Display tooltip, respecting ensime tooltip options."
  (if ensime-graphical-tooltips
      (tooltip-show msg tooltip-use-echo-area)
    (message msg)))


(defun ensime-tooltip-handler (event)
  "Hook function to display a help tooltip. If an error
   or warning overlay exists at point, show the description
   of that error or warning. Otherwise try to inspect the
   type of the expression under the cursor."
  (when (and (eventp event)
             ensime-mode
             (ensime-connected-p)
             (ensime-analyzer-ready)
             (posn-point (event-end event)))

    (let* ((point (posn-point (event-end event)))
           (external-pos (ensime-externalize-offset point))
           (ident (tooltip-identifier-from-point point))
           (note-overlays (ensime-overlays-at point))
           (val-at-pt (ensime-db-tooltip point)))

      (cond

       ;; If debugger is active and we can get the value of the symbol
       ;; at the point, show it in the tooltip.
       (val-at-pt (ensime-tooltip-show-message val-at-pt) t)

       ;; If error or warning overlays exist,
       ;; show that message..
       (note-overlays (progn
                        (ensime-tooltip-show-message
                         (overlay-get (car note-overlays) 'help-echo))
                        t))


       ;; Otherwise show a type hint..
       ((and ident ensime-tooltip-type-hints)
        (progn
          (ensime-eval-async
           `(swank:type-at-point ,buffer-file-name ,external-pos)
           #'(lambda (type)
               (when type
                 (let ((msg (ensime-type-full-name-with-args type)))
                   (ensime-tooltip-show-message msg)
                   ))))
          t
          )))
      )))




;;;;;; Modeline

;; Setup the custom ensime modeline handler
(add-to-list 'minor-mode-alist
             '(ensime-mode (:eval (ensime-modeline-string))))

(defun ensime-modeline-string ()
  "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
  (when ensime-mode
    (condition-case err
	(let ((conn (ensime-connection-or-nil)))
	  (cond ((and ensime-mode (not conn))
		 (cond
		  ((ensime-owning-server-process-for-source-file buffer-file-name)
		   " [ENSIME: (Starting)]")
		  (t " [ENSIME: (Disconnected)]")))

		((and ensime-mode (ensime-connected-p conn))
		 (concat " ["
			 (or (plist-get (ensime-config conn) :name)
			     "ENSIME: (Connected)")
			 (let ((status (ensime-modeline-state-string conn))
			       (unready (not (ensime-analyzer-ready conn))))
			   (cond (status (concat " (" status ")"))
				 (unready " (Analyzing)")
				 (t "")))
			 (concat (format " : %s/%s"
					 (ensime-num-errors conn)
					 (ensime-num-warnings conn)))
			 "]"))
		(ensime-mode " [ENSIME: (Dead Connection)]")
		))
      (error (progn
	       " [ENSIME: wtf]"
	       )))))

(defun ensime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
	 (format "%s" (process-status conn)))
	((let ((pending (length (ensime-rex-continuations conn))))
	   (cond ((zerop pending) nil)
		 (t (format "%s" pending)))))))

(defun ensime--age-file (file)
  (float-time
   (time-subtract (current-time)
		  (nth 5 (or (file-attributes (file-truename file))
			     (file-attributes file))))))

(defun ensime--get-cache-dir (config)
  (let ((cache-dir (plist-get config :cache-dir)))
    (unless cache-dir
      (error "Cache dir in ensime configuration file appears to be unset"))
    cache-dir))

(defun ensime--get-root-dir (config)
  (let ((root-dir (plist-get config :root-dir)))
    (unless root-dir
      (error "Root dir in ensime configuration file appears to be unset"))
    root-dir))

(defun ensime--get-name (config)
  (let ((name (plist-get config :name)))
    (unless name
      (error "Name in ensime configuration file appears to be unset"))
    name))

;; Startup

(defun ensime-update ()
  "Install the most recent version of ENSIME server."
  (interactive)
    (let* ((config-file (ensime-config-find))
           (config (ensime-config-load config-file))
           (scala-version (plist-get config :scala-version)))
      (ensime--update-server scala-version `(lambda () (message "ENSIME server updated.")))))

(defun ensime--maybe-update-and-start (&optional host port)
  (if (and host port)
      ;; When both host and port are provided, we assume we're connecting to
      ;; an existing, listening server.
      (ensime--retry-connect nil host (lambda () port) config cache-dir)
    (let* ((config-file (ensime-config-find))
           (config (ensime-config-load config-file))
           (scala-version (plist-get config :scala-version)))
      (if (file-exists-p (ensime--classpath-file scala-version))
          (ensime--1 config-file)
        (ensime--update-server scala-version `(lambda () (ensime--1 ,config-file)))))))

(defun* ensime--1 (config-file)
  (when (and (ensime-source-file-p) (not ensime-mode))
    (ensime-mode 1))
  (let* ((config (ensime-config-load config-file))
         (root-dir (ensime--get-root-dir config) )
         (cache-dir (file-name-as-directory (ensime--get-cache-dir config)))
         (name (ensime--get-name config))
         (scala-version (plist-get config :scala-version))
         (server-env (or (plist-get config :server-env) ensime-default-server-env))
         (buffer (or (plist-get config :buffer) (concat ensime-default-buffer-prefix name)))
         (server-java (plist-get config :java-home))
         (server-flags (or (plist-get config :java-flags) ensime-default-java-flags)))
    (make-directory cache-dir 't)

    (let* ((server-proc
            (ensime--maybe-start-server
             (generate-new-buffer-name (concat "*" buffer "*"))
             scala-version server-flags
             (list* (concat "JDK_HOME=" server-java)
                    (concat "JAVA_HOME=" server-java)
                    server-env)
             config-file
             cache-dir))
           (host "127.0.0.1")
           (port (lambda () (ensime-read-swank-port
                             (concat cache-dir "/port")))))

      ;; Surface the server buffer so user can observe the startup progress.
      (display-buffer-at-bottom (process-buffer server-proc) nil)

      ;; Store the config on the server process so we can identify it later.
      (process-put server-proc :ensime-config config)
      (push server-proc ensime-server-processes)
      (ensime--retry-connect server-proc host port config cache-dir))))


;; typecheck continually when idle

(defvar ensime-idle-typecheck-timer nil
  "Timer called when emacs is idle")

(defvar ensime-last-change-time 0
  "Time of last buffer change")

(defun ensime-idle-typecheck-set-timer ()
  (when (timerp ensime-idle-typecheck-timer)
    (cancel-timer ensime-idle-typecheck-timer))
  (setq ensime-idle-typecheck-timer
        (run-with-timer nil
                        ensime-typecheck-idle-interval
                        'ensime-idle-typecheck-function)))

(defun ensime-after-change-function (start stop len)
  (set (make-local-variable 'ensime-last-change-time) (float-time)))

(defun ensime-idle-typecheck-function ()
  (when (and ensime-typecheck-when-idle
             (ensime-connected-p)
             (ensime-analyzer-ready))
    (let* ((now (float-time))
           (last-typecheck (ensime-last-typecheck-run-time (ensime-connection)))
           (earliest-allowed-typecheck (+ last-typecheck ensime-typecheck-interval)))
      (when (and (>= now (+ ensime-last-change-time ensime-typecheck-idle-interval))
                 (>= now earliest-allowed-typecheck)
                 (< last-typecheck ensime-last-change-time))
        (ensime-typecheck-current-file t)
        (ensime-sem-high-refresh-hook)))))

(defun ensime-reload ()
  "Re-initialize the project with the current state of the config file.
Analyzer will be restarted. All source will be recompiled."
  (interactive)
  (let* ((conn (ensime-connection))
	 (current-conf (ensime-config conn))
	 (force-dir (plist-get current-conf :root-dir))
	 (config (ensime-config-load (ensime-config-find force-dir) force-dir)))
    (when (not (null config))
      (ensime-set-config conn config)
      (ensime-init-project conn))))

(defun ensime--maybe-start-server (buffer scala-version flags env config-file cache-dir)
  "Return a new or existing server process."
  (let ((existing (comint-check-proc buffer)))
    (if existing existing
      (ensime--start-server buffer scala-version flags env config-file cache-dir))))

(defun ensime--user-directory ()
  (file-name-as-directory
   (expand-file-name "ensime" user-emacs-directory)))

(defun ensime--classpath-file (scala-version)
  (expand-file-name
   (format "classpath_%s_%s"
           scala-version
           (ensime--select-server-version scala-version))
   (ensime--user-directory)))

(defun ensime--update-sentinel (process event scala-version on-success-fn)
  (cond
   ((equal event "finished\n")
    (let ((classpath-file (ensime--classpath-file scala-version)))
      (if (file-exists-p classpath-file)
          (progn
            (when-let
             (win (get-buffer-window (process-buffer process)))
             (delete-window win))
            (kill-buffer (process-buffer process))
            (funcall on-success-fn))
        (message "Could not create classpath file %s" classpath-file))))
   (t
    (message "Process %s exited: %s" process event))))

(defun ensime--update-server (scala-version on-success-fn)
  (with-current-buffer (get-buffer-create (generate-new-buffer-name "*ensime-update*"))
    (let* ((default-directory (file-name-as-directory
                               (make-temp-file "ensime_update_" t)))
           (classpath-file (ensime--classpath-file scala-version))
           (buildfile (concat default-directory "build.sbt"))
           (buildcontents (ensime--create-sbt-start-script scala-version))
           (buildpropsfile (concat default-directory "project/build.properties")))

      (when (file-exists-p classpath-file) (delete-file classpath-file))
      (make-directory (file-name-directory classpath-file) t)
      (ensime-write-to-file buildfile buildcontents)
      (ensime-write-to-file buildpropsfile "sbt.version=0.13.7\n")

      (if (executable-find ensime-sbt-command)
          (let ((process (start-process "*ensime-update*" (current-buffer)
                                        ensime-sbt-command "saveClasspath" "clean")))
            (display-buffer-at-bottom (current-buffer) nil)
            (set-process-sentinel process
                                  `(lambda (process event)
                                     (ensime--update-sentinel process
                                                              event
                                                              ,scala-version
                                                              ',on-success-fn)))
            (message "Updating ENSIME server..."))
        (error "sbt command not found")))))

(defvar ensime-server-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun ensime--start-server (buffer scala-version flags user-env config-file cache-dir)
  "Start an ensime server in the given buffer and return the created process.
BUFFER is the buffer to receive the server output.
FLAGS is a list of JVM flags.
USER-ENV is a list of environment variables.
CACHE-DIR is the server's persistent output directory."
  (message "ENSIME server starting...")
  (with-current-buffer (get-buffer-create buffer)
    (comint-mode)
    (let* ((default-directory cache-dir)
           (classpath-file (ensime--classpath-file scala-version))
           (classpath (ensime-read-from-file classpath-file))
           (process-environment (append user-env process-environment))
           (java-command (concat (getenv "JAVA_HOME") "/bin/java"))
           (args (-flatten (list
                            "-classpath" classpath
                            "-Dscala.usejavacp=true"
                            flags
                            (concat "-Densime.config=" (expand-file-name config-file))
                            "org.ensime.server.Server"))))

      (set (make-local-variable 'comint-process-echoes) nil)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)

      (insert (format "Starting ENSIME server: %s %s\n"
                      java-command
                      (mapconcat 'identity args " ")))
      (if (executable-find java-command)
          (comint-exec (current-buffer) buffer java-command nil args)
        (error "java command %s not found" java-command))
      ;; Make sure we clean up nicely (required on Windows, or port files won't
      ;; be removed).
      (add-hook 'kill-emacs-hook 'ensime-interrupt-all-servers)
      (add-hook 'kill-buffer-hook 'ensime-interrupt-buffer-process nil t)
      (let ((proc (get-buffer-process (current-buffer))))
        (ensime-set-query-on-exit-flag proc)
        (run-hooks 'ensime-server-process-start-hook)
        proc))))

(defun ensime--select-server-version(scala-version) "0.9.10-SNAPSHOT")

(defun ensime--create-sbt-start-script (scala-version)
  ;; emacs has some weird case-preservation rules in regexp replace
  ;; see http://github.com/magnars/s.el/issues/62
  (s-replace-all (list (cons "_scala_version_" scala-version)
                       (cons "_server_version_" (ensime--select-server-version
                                                 scala-version))
                       (cons "_classpath_file_" (ensime--classpath-file scala-version)))
                 ensime--sbt-start-template))


(defconst ensime--sbt-start-template
"
import sbt._
import IO._
import java.io._

scalaVersion := \"_scala_version_\"

resolvers += Resolver.sonatypeRepo(\"snapshots\")

resolvers += \"Typesafe repository\" at \"http://repo.typesafe.com/typesafe/releases/\"

resolvers += \"Akka Repo\" at \"http://repo.akka.io/repository\"

libraryDependencies += \"org.ensime\" %% \"ensime\" % \"_server_version_\"

// guaranteed to exist when started from emacs
val JavaTools = new File(sys.env(\"JAVA_HOME\"), \"/lib/tools.jar\")

unmanagedClasspath in Runtime += { Attributed.blank(JavaTools) }

val saveClasspathTask = TaskKey[Unit](\"saveClasspath\", \"Save the classpath to a file\")

saveClasspathTask := {
  val managed = (managedClasspath in Runtime).value.map(_.data.getAbsolutePath)
  val unmanaged = (unmanagedClasspath in Runtime).value.map(_.data.getAbsolutePath)
  val out = file(\"_classpath_file_\")
  write(out, (unmanaged ++ managed).mkString(File.pathSeparator))
}
")

(defun ensime-shutdown()
  "Request that the current ENSIME server kill itself."
  (interactive)
  (when (ensime-connected-p)
    (ensime-quit-connection (ensime-connection))))

(defun ensime-configured-project-root ()
  "Return root path of the current project as defined in the
config file and stored in the current connection. Nil is returned
if there is no active connection, or if the project root was not
defined."
  (when (ensime-connected-p)
    (let ((config (ensime-config (ensime-connection))))
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

(defun ensime--retry-connect (server-proc host port-fn config cache-dir)
  "When application of port-fn yields a valid port, connect to the port at the
 given host. Otherwise, schedule ensime--retry-connect for re-execution after 6
 seconds."
  (cond (ensime--abort-connection
	 (setq ensime--abort-connection nil)
	 (message "Aborted"))
	((and server-proc (eq (process-status server-proc) 'exit))
	 (message "Failed to connect: server process exited."))
	(t
	 (let ((port (funcall port-fn)))
	   (if port
	       (progn
		 (ensime--connect host port config)
		 ;; Kill the window displaying the server buffer if it's still
		 ;; visible.
		 (when-let
		  (win (get-buffer-window (process-buffer server-proc)))
		  (delete-window win)))
	     (run-at-time
	      "6 sec" nil 'ensime-timer-call 'ensime--retry-connect
	      server-proc host port-fn config cache-dir))))))

(defun ensime--connect (host port config)
  (let ((c (ensime-connect host port)))
    (ensime-set-config c config)
    (let ((ensime-dispatching-connection c))
      (ensime-eval-async
       '(swank:connection-info)
       (ensime-curry #'ensime-handle-connection-info c)))))

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

(defun ensime-init-project (conn)
  "Notify the server that we are ready for project events."
  (cond
   ;; TODO(back_compat)
   ((version<= (ensime-protocol-version conn) "0.8.9")
    (ensime-eval-async `(swank:init-project (:name "NA")) 'identity))

   (t (ensime-eval-async `(swank:init-project) 'identity))))

(provide 'ensime-mode)

;; Local Variables:
;; no-byte-compile: t
;; End:
