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


;; Event Handling

(defun ensime-db-handle-event (evt)
  (case (plist-get evt :type)
    (step (ensime-db-handle-step evt))
    (breakpoint (ensime-db-handle-break-hit evt))
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

(defun ensime-db-handle-step (evt)
  (ensime-db-set-debug-marker
   (plist-get evt :file)
   (plist-get evt :line)))

(defun ensime-db-handle-break-hit (evt)
  (ensime-db-set-debug-marker
   (plist-get evt :file)
   (plist-get evt :line)))


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


(defun ensime-db-refresh-breakpoints ()
  "Refresh all breakpoints from server."
  (ensime-db-clear-breakpoint-overlays)
  (let ((bp-locs (ensime-rpc-debug-list-breakpoints)))
    (dolist (pos bp-locs)
      (let ((file (ensime-pos-file pos))
	    (line (ensime-pos-line pos)))
	(when (and (stringp file) (integerp line))
	  (when-let (ov (ensime-make-overlay-at
			 file line nil nil
			 "Breakpoint"
			 'ensime-breakpoint-face))
	    (push ov ensime-db-breakpoint-overlays)))))))


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




;; User Commands

(defun ensime-db-next ()
  "Cause debugger to go to next line, without stepping into
method invocations."
  (interactive)
  (ensime-rpc-debug-next))

(defun ensime-db-step ()
  "Cause debugger to go to next line, stepping into
method invocations."
  (interactive)
  (ensime-rpc-debug-step))

(defun ensime-db-continue ()
  "Continue stopped debugger."
  (interactive)
  (ensime-rpc-debug-continue))

(defun ensime-db-run ()
  "Start debugging the current program."
  (interactive)
  (ensime-rpc-debug-continue))

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
       (add-hook 'kill-buffer-hook 'ensime-db-clear-breakpoint-overlays nil t)
       (add-hook 'kill-buffer-hook 'ensime-db-clear-marker-overlays nil t)
       (ensime-db-clear-breakpoint-overlays)
       (ensime-db-clear-marker-overlays)

       (insert "Starting debug vm...")
       (ensime-rpc-debug-start cmd-line)
       ))))





(provide 'ensime-debug)
