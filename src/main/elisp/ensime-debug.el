;;; ensime-debug.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(defgroup ensime-db nil
  "Customization of ensime debugger support."
  :group 'ensime
  :prefix 'ensime-db)

(defface ensime-breakpoint-face
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)

(defface ensime-pending-breakpoint-face
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)

(defface ensime-marker-face
  '((((class color) (background dark)) (:background "DarkGoldenrod4"))
    (((class color) (background light)) (:background "DarkGoldenrod2"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)

(defvar ensime-db-default-main-args nil
  "History of arguments passed to main class.")

(defvar ensime-db-default-main-class nil
  "History of main class to debugger.")


(defvar ensime-db-history nil
  "History of argument lists passed to jdb.")

(defvar ensime-db-buffer-name "*ensime-debug-session*")

(defvar ensime-db-active-thread-id nil
  "The unique id of the which is currently receiving debug
 commands.")


;; Event Handling

(defun ensime-db-handle-event (evt)
  (case (plist-get evt :type)
    (start (ensime-db-handle-start evt))
    (step (ensime-db-handle-step evt))
    (breakpoint (ensime-db-handle-break-hit evt))
    (death (ensime-db-handle-shutdown evt))
    (disconnect (ensime-db-handle-shutdown evt))
    (exception (ensime-db-handle-exception evt))
    (otherwise (ensime-db-handle-unknown-event evt))
    ))

(defun ensime-db-handle-unknown-event (evt)
  (message "Unknown event: %s" evt))

(defun ensime-db-handle-exception (evt)
  (message "Exception on thread %s : %s"
	   (plist-get evt :thread-id)
	   (plist-get evt :exception)
	   ))

(defun ensime-db-handle-start (evt)
  (message "Debug VM started. Set breakpoints and then execute ensime-db-run."))

(defun ensime-db-handle-step (evt)
  (ensime-db-set-debug-marker
   (plist-get evt :file)
   (plist-get evt :line))
  (message "Suspended thread %s at %s : %s"
	   (plist-get evt :thread-id)
	   (plist-get evt :file)
	   (plist-get evt :line)))

(defun ensime-db-handle-break-hit (evt)
  (setq ensime-db-active-thread-id
	(plist-get evt :thread-id))
  (ensime-db-set-debug-marker
   (plist-get evt :file)
   (plist-get evt :line)))

(defun ensime-db-handle-shutdown (evt)
  (message "Debug VM Quit")
  (setq ensime-db-active-thread-id nil))


;; UI

(defun ensime-db-set-debug-marker (file line)
  "Open location in a new window."
  (ensime-db-clear-marker-overlays)
  (when-let (ov (ensime-make-overlay-at
		 file line nil nil
		 "Debug Marker"
		 'ensime-marker-face))
    (push ov ensime-db-marker-overlays))

  (ensime-goto-source-location
   (list :file file :line line)
   'window))


(defun ensime-db-create-breapoint-overlays (positions face)
  (dolist (pos positions)
    (let ((file (ensime-pos-file pos))
	  (line (ensime-pos-line pos)))
      (when (and (stringp file) (integerp line))
	(when-let (ov (ensime-make-overlay-at
		       file line nil nil
		       "Breakpoint"
		       face))
	  (push ov ensime-db-breakpoint-overlays))))))


(defun ensime-db-refresh-breakpoints ()
  "Refresh all breakpoints from server."
  (ensime-db-clear-breakpoint-overlays)
  (let* ((bps (ensime-rpc-debug-list-breakpoints))
	 (active (plist-get bps :active))
	 (pending (plist-get bps :pending)))
    (ensime-db-create-breapoint-overlays
     active 'ensime-breakpoint-face)
    (ensime-db-create-breapoint-overlays
     pending 'ensime-pending-breakpoint-face)))


(defvar ensime-db-breakpoint-overlays '())

(defun ensime-db-clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-breakpoint-overlays)
  (setq ensime-db-breakpoint-overlays '()))


(defvar ensime-db-marker-overlays '())

(defun ensime-db-clear-marker-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-marker-overlays)
  (setq ensime-db-marker-overlays '()))

(defmacro* ensime-db-with-active-thread ((tid-sym) &rest body)
  `(if ensime-db-active-thread-id
       (let ((,tid-sym ensime-db-active-thread-id))
	 ,@body)
     (message "No active debug thread.")))


(defun ensime-db-value-short-name (val)
  "Get a short, pretty name for a debug value."
  (case (plist-get val :val-type)
    (prim (format "%s"
		  (plist-get val :value)))
    (obj (format "Instance of %s"
		 (plist-get val :type-name)))
    (arr (format "Array[%s] of length %s"
		 (plist-get val :element-type-name)
		 (plist-get val :length)))
    (str (plist-get val :string-value))
    (otherwise (format "%s" val))
    ))


(defun ensime-db-value-p (val)
  (not (null (plist-get val :val-type))))


(defun ensime-db-ui-insert-field (f)
  (insert (format "%s : "
		  (plist-get f :name)))
  (ensime-insert-with-face
   (plist-get f :type-name)
   'font-lock-type-face)
  (when-let (val (plist-get f :value))
    (insert
     (format
      " = %s"
      (ensime-db-value-short-name val))))
  (insert "\n\n"))



(defun ensime-db-ui-insert-value (val)
  (case (plist-get val :val-type)

    (prim (insert (format "%s : %s"
			  (plist-get val :value)
			  (plist-get val :type-name))))

    (obj (progn
	   (insert (format "Instance of %s\n"
			   (plist-get val :type-name)))
	   (ensime-insert-with-face
	    "\n------------------------\n\n"
	    'font-lock-comment-face)
	   (dolist (f (plist-get val :fields))
	     (ensime-db-ui-insert-field f)
	     )))


    (arr (progn
	   (insert (format "Array[%s] of length %s\n"
			   (plist-get val :element-type-name)
			   (plist-get val :length)))
	   (ensime-insert-with-face
	    "\n------------------------\n\n"
	    'font-lock-comment-face)
	   (let ((i 0)
		 (limit (min (plist-get val :length) 10)))
	     (while (< i limit)
	       (insert (format "[%s]\n\n" i))
	       (incf i)))))

    (str (progn
	   (ensime-insert-with-face (format "\"%s\"\n"
					    (plist-get val :string-value))
				    'font-lock-string-face)
	   (ensime-insert-with-face
	    "\n------------------------\n\n"
	    'font-lock-comment-face)
	   (dolist (f (plist-get val :fields))
	     (ensime-db-ui-insert-field f)
	     )))


    (otherwise (format "%s" val))
    ))


(defvar ensime-db-ui-value-handler
  (list
   :init (lambda (info)
	   (ensime-db-ui-insert-value info))
   :update (lambda (info))
   :help-text "Press q to quit."
   :keymap ()
   ))



;; User Commands

(defun ensime-db-value-for-name-at-point (p)
  "Get the value of the symbol at point."
  (when ensime-db-active-thread-id
    (when-let (sym (ensime-sym-at-point p))
      (ensime-db-with-active-thread (tid)
				    (ensime-rpc-debug-value-for-name
				     tid (plist-get sym :name))
				    ))))

(defun ensime-db-inspect-value-at-point (p)
  "Get the value of the symbol at point."
  (interactive (list (point)))
  (let ((val (ensime-db-value-for-name-at-point (point))))
    (if val (ensime-ui-show-nav-buffer "*ensime-debug-value*" val t)
      (message "Nothing to inspect."))))

(defun ensime-db-next ()
  "Cause debugger to go to next line, without stepping into
 method invocations."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-next tid)))

(defun ensime-db-step ()
  "Cause debugger to go to next line, stepping into
 method invocations."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-step tid)))

(defun ensime-db-step-out ()
  "Cause debugger to go to next line, stepping out of
 method invocations."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-step-out tid)))

(defun ensime-db-continue ()
  "Continue stopped debugger."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-continue tid)))

(defun ensime-db-run ()
  "Start debugging the current program."
  (interactive)
  (if (ensime-rpc-debug-active-vm)
      (ensime-rpc-debug-run)
    (ensime-db-start)))

(defun ensime-db-set-break (f line)
  "Set a breakpoint in the current source file at point."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (ensime-rpc-debug-set-break f line)
  (ensime-db-refresh-breakpoints))

(defun ensime-db-clear-break (f line)
  "Clear breakpoint."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (ensime-rpc-debug-clear-break f line)
  (ensime-db-refresh-breakpoints))

(defun ensime-db-clear-all-breaks ()
  "Clear all breakpoints."
  (interactive)
  (ensime-rpc-debug-clear-all-breaks)
  (ensime-db-refresh-breakpoints))

(defun ensime-db-quit ()
  "Stop debugging the current program. Kills the debug buffer."
  (interactive)
  (ensime-rpc-debug-stop)
  (kill-buffer ensime-db-buffer-name))

(defun ensime-db-get-cmd-line ()
  "Get the command needed to launch a debugger, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (let* ((debug-class
	  (ensime-strip-dollar-signs
	   (ensime-completing-read-path
	    "Qualified name of class to debug: "
	    ensime-db-default-main-class)))
	 (debug-args (read-string
		      "Commandline arguments: "
		      ensime-db-default-main-args)))
    (setq ensime-db-default-main-class debug-class)
    (setq ensime-db-default-main-args debug-args)
    (concat debug-class " " debug-args)))

(defun ensime-db-start ()
  "Run a Scala interpreter in an Emacs buffer"
  (interactive)

  (ensime-with-conn-interactive
   conn
   (let ((root-path (or (ensime-configured-project-root) "."))
	 (cmd-line (ensime-db-get-cmd-line)))

     (save-selected-window
       (switch-to-buffer-other-window
	(get-buffer-create ensime-db-buffer-name))
       (setq ensime-buffer-connection conn)
       (add-hook 'kill-buffer-hook 'ensime-db-clear-marker-overlays nil t)
       (ensime-db-clear-marker-overlays)
       (insert "Starting debug vm...")
       (ensime-rpc-debug-start cmd-line)
       ))))





(provide 'ensime-debug)
