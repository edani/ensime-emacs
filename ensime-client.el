;;; ensime-client.el --- RPC calls and support functions

;;; `ensime-rex' is the RPC primitive which is used to implement both
;;; `ensime-eval' and `ensime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* ensime-rex ((&rest saved-vars)
                       sexp
                       &rest continuations)
  "(ensime-rex (VAR ...) SEXP CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort REASON).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (ensime-dispatch-event
        (list :swank-rpc ,sexp
              (lambda (,result)
                (destructure-case ,result
                                  ,@continuations)))))))

(put 'ensime-rex 'lisp-indent-function 2)

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar ensime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun ensime-eval (sexp)
  "Evaluate EXPR on the superior Lisp and return the result."
  (let* ((tag (gensym (format "ensime-result-%d-sym"
                              (1+ (ensime-continuation-counter)))))
         (ensime-stack-eval-tags (cons tag ensime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (ensime-rex (tag sexp)
           sexp

         ((:ok value)
          (if (not (member tag ensime-stack-eval-tags))
              (message
               "Reply to canceled synchronous eval request tag=%S sexp=%S"
               tag sexp)
            (throw tag (list #'identity value))))

         ((:abort code reason)
          (message
           (format
            "Synchronous RPC Aborted: %s" reason))
          (throw tag (list #'identity nil))))

       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (ensime-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (accept-process-output nil 1 0)))))))


(defun ensime-eval-async (sexp &optional cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (ensime-rex (cont (buffer (current-buffer)))
      sexp
    ((:ok result)
     (when cont
       (if (buffer-live-p buffer)
           (progn
             (set-buffer buffer)
             (funcall cont result))
         (message
          "ENSIME: Asynchronous return could not find originating buffer.")
         )))
    ((:abort code reason)
     (message "Asynchronous RPC Aborted: %s" reason)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; ensime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :ensime-eval-async)


;;; RPC functions

(defun ensime-rpc-method-bytecode (file line)
  (ensime-eval
   `(swank:method-bytecode ,file ,line)))

(defun ensime-rpc-debug-active-vm ()
  (ensime-eval
   `(swank:debug-active-vm)))

(defun ensime-rpc-debug-backtrace (thread-id index count)
  (ensime-eval
   `(swank:debug-backtrace ,thread-id ,index ,count)))

(defun ensime-rpc-async-debug-backtrace (thread-id index count continue)
  (ensime-eval-async
   `(swank:debug-backtrace ,thread-id ,index ,count) continue))

(defun ensime-rpc-debug-locate-name (thread-id name)
  (ensime-eval
   `(swank:debug-locate-name ,thread-id ,name)))

(defun ensime-rpc-debug-value (location)
  (ensime-eval
   `(swank:debug-value ,location)))

(defun ensime-rpc-debug-to-string (thread-id location)
  (ensime-eval
   `(swank:debug-to-string ,thread-id ,location)))

(defun ensime-rpc-debug-set-value (location new-val)
  (ensime-eval
   `(swank:debug-set-value ,location ,new-val)))

(defun ensime-rpc-debug-start (command-line)
  (ensime-eval
   `(swank:debug-start ,command-line)))

(defun ensime-rpc-debug-attach (hostname port)
  (ensime-eval
   `(swank:debug-attach ,hostname ,port)))

(defun ensime-rpc-debug-stop ()
  (ensime-eval
   `(swank:debug-stop)))

(defun ensime-rpc-debug-next (thread-id)
  (ensime-eval
   `(swank:debug-next ,thread-id)))

(defun ensime-rpc-debug-continue (thread-id)
  (ensime-eval
   `(swank:debug-continue ,thread-id)))

(defun ensime-rpc-debug-run ()
  (ensime-eval
   `(swank:debug-run)))

(defun ensime-rpc-debug-step (thread-id)
  (ensime-eval
   `(swank:debug-step ,thread-id)))

(defun ensime-rpc-debug-step-out (thread-id)
  (ensime-eval
   `(swank:debug-step-out ,thread-id)))

(defun ensime-rpc-debug-list-breakpoints ()
  (ensime-eval
   `(swank:debug-list-breakpoints)))

(defun ensime-rpc-debug-set-break (file line)
  (ensime-eval
   `(swank:debug-set-break ,file ,line)))

(defun ensime-rpc-debug-clear-break (file line)
  (ensime-eval
   `(swank:debug-clear-break ,file ,line)))

(defun ensime-rpc-debug-clear-all-breaks ()
  (ensime-eval
   `(swank:debug-clear-all-breaks)))

(defun ensime-rpc-symbol-at-point ()
  (ensime-eval
   `(swank:symbol-at-point ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-repl-config ()
  "Get the configuration information needed to launch the scala interpreter
with the current project's dependencies loaded. Returns a property list."
  (ensime-eval
   `(swank:repl-config)))

(defun ensime-rpc-remove-file (file-name)
  (ensime-eval `(swank:remove-file ,file-name)))

(defun ensime-rpc-async-typecheck-file (file-name continue)
  (ensime-eval-async `(swank:typecheck-file ,file-name) continue))

(defun ensime-rpc-async-typecheck-file-with-contents (file-name contents continue)
  (ensime-eval-async `(swank:typecheck-file ,file-name ,contents)
                     continue))

(defun ensime-rpc-async-typecheck-all (continue)
  (ensime-eval-async `(swank:typecheck-all) continue))

(defun ensime-rpc-async-builder-init (continue)
  (ensime-eval-async `(swank:builder-init) continue))

(defun ensime-rpc-async-builder-update (file-names continue)
  (ensime-eval-async `(swank:builder-update-files ,file-names) continue))

(defun ensime-rpc-async-format-files (file-names continue)
  (ensime-eval-async `(swank:format-source ,file-names) continue))

(defun ensime-rpc-expand-selection (file-name start end)
  (ensime-internalize-offset-fields
   (ensime-eval `(swank:expand-selection
		  ,file-name
		  ,(ensime-externalize-offset start)
		  ,(ensime-externalize-offset end)))
   :start
   :end
   ))


(defun ensime-rpc-import-suggestions-at-point (names max-results)
  (ensime-eval
   `(swank:import-suggestions
     ,buffer-file-name
     ,(ensime-computed-point)
     ,names
     ,max-results
     )))

(defun ensime-rpc-async-public-symbol-search
  (names max-results continue)
  (ensime-eval-async
   `(swank:public-symbol-search
     ,names
     ,max-results
     ) continue))

(defun ensime-rpc-uses-of-symbol-at-point ()
  (ensime-eval
   `(swank:uses-of-symbol-at-point
     ,buffer-file-name
     ,(ensime-computed-point)
     )))

(defun ensime-rpc-package-member-completions (path &optional prefix)
  (ensime-eval
   `(swank:package-member-completion ,path ,(or prefix ""))))

(defun ensime-rpc-get-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:type-by-id ,id))))

(defun ensime-rpc-get-type-by-name (name)
  (ensime-eval
   `(swank:type-by-name ,name)))

(defun ensime-rpc-get-type-by-name-at-point (name)
  (ensime-eval
   `(swank:type-by-name-at-point
     ,name ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-get-type-at-point ()
  (ensime-eval
   `(swank:type-at-point ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-inspect-type-at-point ()
  (ensime-eval
   `(swank:inspect-type-at-point ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-inspect-type-at-range (&optional range)
  (ensime-eval
   `(swank:inspect-type-at-point ,buffer-file-name
                                 ,(or range (ensime-computed-range)))))

(defun ensime-rpc-inspect-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:inspect-type-by-id ,id))))

(defun ensime-rpc-inspect-package-by-path (path)
  (ensime-eval
   `(swank:inspect-package-by-path ,path)))

(defun ensime-rpc-peek-undo ()
  (ensime-eval
   `(swank:peek-undo)))

(defun ensime-rpc-exec-undo (id)
  (ensime-eval
   `(swank:exec-undo ,id)))

(defun ensime-rpc-refactor-prepare
  (proc-id refactor-type params non-interactive continue blocking)
  (if blocking
      (ensime-eval
       `(swank:prepare-refactor
	 ,proc-id ,refactor-type ,params ,(not non-interactive)))
    (ensime-eval-async
     `(swank:prepare-refactor
       ,proc-id ,refactor-type ,params ,(not non-interactive)) continue)))

(defun ensime-rpc-refactor-exec (proc-id refactor-type continue)
  (ensime-eval-async `(swank:exec-refactor ,proc-id , refactor-type) continue))

(defun ensime-rpc-refactor-cancel (proc-id)
  (ensime-eval-async `(swank:cancel-refactor ,proc-id) #'identity))


(defun ensime-rpc-shutdown-server ()
  (ensime-eval `(swank:shutdown-server)))

(defun ensime-rpc-symbol-designations (file start end requested-types continue)
  (ensime-eval-async `(swank:symbol-designations ,file ,start ,end ,requested-types)
		     continue))

(defun ensime-rpc-get-call-completion (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:call-completion ,id))))

(defun ensime-rpc-completions-at-point (&optional max-results case-sens)
  (ensime-eval
   `(swank:completions
     ,buffer-file-name
     ,(ensime-computed-point)
     ,(or max-results 0)
     ,case-sens
     t ;; reload
     )))


(provide 'ensime-client)

;; Local Variables:
;; no-byte-compile: t
;; End:
