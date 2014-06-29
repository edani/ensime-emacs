;;; ensime-event.el ---  Protocol event handler (the guts)

;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from the ENSIME server.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from the ENSIME server don't.

(defvar ensime-event-hooks)

(defun ensime-dispatch-event (event &optional process)
  (let ((ensime-dispatching-connection (or process (ensime-connection))))
    (or (run-hook-with-args-until-success 'ensime-event-hooks event)
	(destructure-case event
                          ((:swank-rpc form continuation)
                           (let ((id (incf (ensime-continuation-counter))))
                             (ensime-send `(:swank-rpc ,form ,id))
                             (push (cons id continuation) (ensime-rex-continuations))
                             ))

                          ((:return value id)
                           (let ((rec (assq id (ensime-rex-continuations))))

                             (cond (rec (setf (ensime-rex-continuations)
                                              (remove rec (ensime-rex-continuations)))
                                        (funcall (cdr rec) value)
                                        (force-mode-line-update t)
                                        (ensime-event-sig :return-value value))
                                   (t
                                    (error "Unexpected reply: %S %S" id value)))))


                          ((:full-typecheck-finished)
                           (when (ensime-awaiting-full-typecheck (ensime-connection))
                             (message "Typecheck finished.")
                             (setf (ensime-awaiting-full-typecheck
                                    (ensime-connection)) nil)
                             (ensime-show-all-errors-and-warnings))
                           (ensime-event-sig :full-typecheck-finished t))

                          ((:compiler-ready)
                           (ensime-handle-compiler-ready)
                           (ensime-event-sig :compiler-ready t))

                          ((:indexer-ready)
                           (ensime-event-sig :indexer-ready t))

                          ((:scala-notes result)
                           (ensime-add-notes 'scala result))

                          ((:java-notes result)
                           (ensime-add-notes 'java result))

                          ((:clear-all-scala-notes)
                           (ensime-clear-notes 'scala))

                          ((:clear-all-java-notes)
                           (ensime-clear-notes 'java))

                          ((:debug-event evt)
                           (ensime-db-handle-event evt)
                           (ensime-event-sig :debug-event evt))

                          ((:channel-send id msg)
                           (ensime-channel-send (or (ensime-find-channel id)
                                                    (error "Invalid channel id: %S %S" id msg))
                                                msg))
                          ((:emacs-channel-send id msg)
                           (ensime-send `(:emacs-channel-send ,id ,msg)))
                          ((:read-from-minibuffer thread tag prompt initial-value)
                           (ensime-read-from-minibuffer-for-swank
                            thread tag prompt initial-value))
                          ((:y-or-n-p thread tag question)
                           (ensime-y-or-n-p thread tag question))
                          ((:emacs-return-string thread tag string)
                           (ensime-send `(:emacs-return-string ,thread ,tag ,string)))
                          ((:new-features features)
                           (setf (ensime-server-features) features))
                          ((:eval-no-wait fun args)
                           (apply (intern fun) args))
                          ((:eval thread tag form-string)
                           (ensime-check-eval-in-emacs-enabled)
                           (ensime-eval-for-lisp thread tag form-string))
                          ((:emacs-return thread tag value)
                           (ensime-send `(:emacs-return ,thread ,tag ,value)))
                          ((:ed what)
                           (ensime-ed what))
                          ((:background-message code detail)
                           (ensime-background-message "%s" detail))
                          ((:reader-error code detail)
                           (ensime-with-popup-buffer
                            ("*Ensime Error*")
                            (princ (format "Invalid protocol message:\n%s\n\n%S"
                                           condition packet))
                            (goto-char (point-min)))
                           (error "Invalid protocol message"))
                          ))))

(defun ensime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (ensime-net-send sexp (ensime-connection)))


(defun ensime-handle-compiler-ready ()
  "Do any work that should be done the first time the analyzer becomes
 ready for requests."
  (let ((conn (ensime-current-connection)))
    (message "ENSIME ready. %s" (ensime-random-words-of-encouragement))
    (setf (ensime-analyzer-ready conn) t)
    (ensime-sem-high-refresh-all-buffers)
    ))

;;; Words of encouragement

(defun ensime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
		  (user-login-name)
		(user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar ensime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "May the source be with you!"
    "Death to null!"
    "Find closure!"
    "May the _ be with you."
    "M-x be_cool"
    "CanBuildFrom[List[Dream], Reality, List[Reality]]"
    ,(format "%s, this could be the start of a beautiful program."
	     (ensime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun ensime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length ensime-words-of-encouragement))
	     ensime-words-of-encouragement)))


(provide 'ensime-event)

;; Local Variables:
;; no-byte-compile: t
;; End:
