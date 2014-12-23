;;; ensime-auto-complete.el
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

(require 'auto-complete)
(require 'yasnippet)

(defcustom ensime-completion-style 'company
  "Should be either 'company or 'auto-complete."
  :type 'symbol
  :group 'ensime-ui)

(defcustom ensime-ac-enable-argument-placeholders t
  "If non-nil, insert placeholder arguments in the buffer on completion."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-ac-override-settings t
    "If non-nil, override auto-complete settings."
    :type 'boolean
    :group 'ensime-ui)

(defcustom ensime-ac-case-sensitive nil
  "If non-nil, omit completions that don't match the case of prefix."
  :type 'boolean
  :group 'ensime-ui)

(defvar ensime-ac-max-results 30
  "Maximum number of completions to request in one call to server.")


(defun ensime-ac-completion-candidates (prefix)
  "Return candidate list."
  (let* ((info
	  (progn
	    (ensime-write-buffer nil t)
	    (ensime-rpc-completions-at-point ensime-ac-max-results
					     ensime-ac-case-sensitive)))
	 (completions (plist-get info :completions)))

    (mapcar (lambda (m)
	      (let* ((type-sig (plist-get m :type-sig))
		     (type-id (plist-get m :type-id))
		     (is-callable (plist-get m :is-callable))
		     (to-insert (plist-get m :to-insert))
		     (name (plist-get m :name))
		     (candidate name))
		(propertize candidate
			    'symbol-name name
			    'type-sig type-sig
			    'type-id type-id
			    'is-callable is-callable
			    'to-insert to-insert
			    'summary (ensime-ac-trunc-summary
				      (ensime-ac-brief-type-sig type-sig))
			    )))
	    completions)
    ))


(defun ensime-ac-trunc-summary (str)
  (let ((len (length str)))
    (if (> len 40)
	(concat (substring str 0 40) "...")
      str)))

(defun ensime-ac-brief-type-sig (type-sig)
  "Return doc for given item."
  ;;(ensime-ac-brief-type-sig '(((("aemon" "Person"))) "Dude"))
  (let* ((sections (car type-sig))
	 (return-type (cadr type-sig)))
    (if sections
	(format "%s: %s"
		(mapconcat
		 (lambda (section)
		   (format "(%s)"
			   (mapconcat
			    (lambda (param-pair)
			      (format "%s: %s" (car param-pair) (cadr param-pair)))
			    section ", ")))
		 sections "=>") return-type)
      return-type)))

(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (ensime-ac-brief-type-sig (get-text-property 0 'type-sig item)))

(defun ensime-ac-candidate-to-insert (item)
  "Return to-insert for given item."
  (get-text-property 0 'to-insert item))

(defun ensime-pt-at-end-of-prev-line ()
  (save-excursion (forward-line -1)
		  (min
		   (- (point) 1)
		   (point-at-eol))))

(defun ensime-ac-completion-prefix ()
  "Starting at current point. Find the point of completion."
  (let ((point (re-search-backward "\\(\\W\\|[\t ]\\)\\([^\\. ]*\\)?"
				   (point-at-bol) t)))
    (if point (1+ point))
    ))


(defun ensime-ac-complete-action (&optional candidate-in)
  "Defines action to perform when user selects a completion candidate.
If the candidate is a callable symbol, add the meta-info about the
params and param types as text-properties of the completed name. This info will
be used later to give contextual help when entering arguments."
  (let* (;; When called by auto-complete-mode, grab from dynamic environment.
	 (candidate (or candidate-in candidate))
	 (name candidate)
	 (type-id (get-text-property 0 'type-id candidate))
	 (is-callable (get-text-property 0 'is-callable candidate))
	 (to-insert (ensime-ac-candidate-to-insert candidate))
	 (name-start-point (- (point) (length name))))

    ;; If an alternate to-insert string is available, delete the
    ;; candidate inserted into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    ;; If this member is callable, use the type-id to lookup call completion
    ;; information to show parameter hints.
    (when is-callable

      (let* ((call-info (ensime-rpc-get-call-completion type-id))
	     (param-sections (ensime-type-param-sections call-info)))
	(when (and call-info param-sections)

	  ;; Insert space or parens depending on the nature of the
	  ;; call
	  (save-excursion
	    (let* ((is-operator
		    (and (= 1 (length param-sections))
			 (= 1 (length (plist-get
				       (car param-sections) :params)))
			 (null (string-match "[A-z]" name)))))
	      (if ensime-ac-enable-argument-placeholders
		  (let ((args (ensime-ac-call-info-argument-list
			       call-info is-operator)))
		    (cond
		     (is-operator (insert (concat " " args)))
		     (t (insert args))))
		(cond
		 (is-operator (insert " "))
		 (t (insert "()"))))))

	  (if (car param-sections)
	      (progn
		;; Save param info as a text properties of the member name..
		(add-text-properties name-start-point
				     (+ name-start-point (length name))
				     (list 'call-info call-info
					   ))

		;; Setup hook function to show param help later..
		(add-hook 'post-command-hook
			  'ensime-ac-update-param-help nil t)
		;; This command should trigger help hook..
		(forward-char))

	    ;; Otherwise, skip to the end
	    (forward-char 2))

	  )))))

(defun ensime-in-string-or-comment (pos)
  "A helper to determine if the text at point is in a string
   or comment, and therefore should not be considered as part
   of a paren-balancing calculation.

   TODO: Currently this relies on font-lock-mode. Could be
   better."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
     (or
      (equal face 'font-lock-doc-face)
      (equal face 'font-lock-string-face)
      (equal face 'font-lock-comment-face)))))

(defun ensime-ac-get-active-param-info ()
  "Search backward from point for the param info of the call that
   we are currently completing."
  (save-excursion
    (catch 'return
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((call-info (get-text-property (point) 'call-info)))
	      (if (and (or (> balance 0)) call-info)
		  (throw 'return (list
				  :name-end-point (point)
				  :call-info call-info))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((info (ensime-ac-get-active-param-info)))
    (if info
	(let* (;; To be used for tooltip positioning..
	       (name-end (plist-get info :name-end-point))
	       (call-info (plist-get info :call-info))
	       (signature (ensime-ac-call-info-signature call-info)))
	  (message signature))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))

(defun ensime--build-yasnippet-for-call (param-sections &optional is-operator)
  "Returns a yasnippet template for a method call, where each argument is a
 tab-stop."
  (concat
    (mapconcat
     (lambda (sect)
       (let* ((params (plist-get sect :params))
	      (is-implicit (plist-get sect :is-implicit))
	      (i 0)
	      (result
	       (concat (if is-operator "" "(")
		       (mapconcat
			(lambda (nm-and-tp)
			  (let ((param-name (car nm-and-tp))
				(type-name
				 (ensime-type-name-with-args
				  (cadr nm-and-tp))))
			  (format
			   "${%s:%s: %s}"
			   (incf i)
			   ;; Escape yasnippet special chars
			   (s-replace "$" "\\$" param-name)
			   (s-replace "$" "\\$" type-name)
			   )))
			params ", ") (if is-operator "" ")"))))
	 (if is-implicit
	     (propertize result 'face font-lock-comment-face)
	   result)
	 ))
     param-sections "=>" )
    "$0"))


(defun ensime-ac-call-info-argument-list (call-info &optional is-operator)
  "Return a pretty string representation of argument list."
  (let ((param-sections (plist-get call-info :param-sections)))
    (mapconcat
     (lambda (sect)
       (let* ((params (plist-get sect :params))
	      (is-implicit (plist-get sect :is-implicit))
	      (result
	       (concat (if is-operator "" "(")
		       (mapconcat
			(lambda (nm-and-tp)
			  (format
			   "%s:%s"
			   (propertize (car nm-and-tp)
				       'face font-lock-variable-name-face)
			   (propertize (ensime-type-name-with-args
					(cadr nm-and-tp))
				       'face font-lock-type-face)
			   ))
			params ", ") (if is-operator "" ")"))))
	 (if is-implicit
	     (propertize result 'face font-lock-comment-face)
	   result)
	 ))
     param-sections "=>" )))


(defun ensime-ac-call-info-signature (call-info)
  "Return a pretty string representation of a call-info object."
  (let ((param-sections (plist-get call-info :param-sections))
	(result-type (plist-get call-info :result-type)))
    (concat
     (ensime-ac-call-info-argument-list call-info)
     " => "
     (propertize
      (ensime-type-name-with-args result-type)
      'face font-lock-type-face)
     )))


(ac-define-source ensime-completions
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-completion-candidates ac-prefix))
    (prefix . ensime-ac-completion-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "f")
    ))


(defun ensime-ac-enable ()
  (when ensime-ac-override-settings
    (make-local-variable 'ac-sources)
	(setq ac-sources '(ac-source-ensime-completions))

	(make-local-variable 'ac-use-comphist)
	(setq ac-use-comphist nil)

	(make-local-variable 'ac-auto-show-menu)
	(setq ac-auto-show-menu 0.5)

	(make-local-variable 'ac-candidates-cache)
	(setq ac-candidates-cache nil)

	(make-local-variable 'ac-auto-start)
	(setq ac-auto-start nil)

	(make-local-variable 'ac-expand-on-auto-complete)
	(setq ac-expand-on-auto-complete t)

	(make-local-variable 'ac-use-fuzzy)
	(setq ac-use-fuzzy nil)

	(make-local-variable 'ac-dwim)
	(setq ac-dwim nil)

	(make-local-variable 'ac-use-quick-help)
	(setq ac-use-quick-help t)

;;    (defvar ac-delete-dups)
	(make-local-variable 'ac-delete-dups)
	(setq ac-delete-dups nil)

	(make-local-variable 'ac-ignore-case)
	(setq ac-ignore-case t)

	(make-local-variable 'ac-trigger-key)
	(ac-set-trigger-key "TAB")

	(auto-complete-mode 1)
  ))

(defun ensime-ac-disable ()
  (auto-complete-mode 0))

(defvar-local ensime--completion-cache nil
  "An optimizing cache for auto-completion. Stores a plist of the form
 '(:prefix 'myPrefi' :candidates ('myPrefix' 'myPreficture' ...))")

(defun ensime-company-complete-or-indent ()
  "A hack to allow use of TAB for completion."
  (interactive)
  (if (and company-mode
	   ;; Always fall back to indentation if we're at b.o.l.
	   (string-match "[^\s-]" (buffer-substring-no-properties
				   (point-at-bol)
				   (point)))
	   (company-manual-begin))
      (company-complete-common)
    (indent-according-to-mode)))

(defun ensime-company-enable ()
  (set (make-local-variable 'company-backends) '(ensime-company))
  (company-mode)
  (yas-minor-mode-on)
  (setq company-idle-delay 0)
  (local-set-key [tab] 'ensime-company-complete-or-indent))

(defun ensime--yasnippet-complete-action (&optional candidate-in)
  "If the candidate is a callable symbol, expand a yasnippet template for the
 argument list."
  (let* (;; When called by auto-complete-mode, grab from dynamic environment.
	 (candidate (or candidate-in candidate))
	 (name candidate)
	 (type-id (get-text-property 0 'type-id candidate))
	 (is-callable (get-text-property 0 'is-callable candidate))
	 (to-insert (ensime-ac-candidate-to-insert candidate))
	 (name-start-point (- (point) (length name))))

    ;; If an alternate to-insert string is available, delete the
    ;; candidate inserted into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    (when is-callable
      (let* ((call-info (ensime-rpc-get-call-completion type-id))
	     (param-sections (ensime-type-param-sections call-info))
	     (is-operator
	      (and (= 1 (length param-sections))
		   (= 1 (length (plist-get
				 (car param-sections) :params)))
		   (null (string-match "[A-z]" name)))))
	(when (and call-info param-sections)
	  (yas-expand-snippet
	   (ensime--build-yasnippet-for-call param-sections is-operator)
	   (point) (point))
	  )))))

(defun ensime--annotate-completions (completions)
  "Maps plist structures to propertized elisp strings."
  (mapcar
   (lambda (m)
     (let* ((type-sig (plist-get m :type-sig))
	    (type-id (plist-get m :type-id))
	    (is-callable (plist-get m :is-callable))
	    (to-insert (plist-get m :to-insert))
	    (name (plist-get m :name))
	    (candidate name))
       (propertize candidate
		   'symbol-name name
		   'type-sig type-sig
		   'type-id type-id
		   'is-callable is-callable
		   'to-insert to-insert
		   ))) completions))

(defun ensime--ask-server-for-completions-async (callback)
  (ensime-write-buffer nil t)
  (ensime-rpc-async-completions-at-point
   ensime-ac-max-results
   ensime-ac-case-sensitive
   (lexical-let ((continuation callback))
     (lambda (info)
       (let* ((prefix (plist-get info :prefix))
	      (candidates (ensime--annotate-completions
			   (plist-get info :completions))))
	 ;; Refresh the cache with latest from server.
	 (setq ensime--completion-cache (list :prefix prefix :candidates candidates))
	 (funcall continuation candidates))))))

(defun ensime--ask-server-for-completions ()
  (ensime-write-buffer nil t)
  (let* ((info
	  (ensime-rpc-completions-at-point
	   ensime-ac-max-results
	   ensime-ac-case-sensitive))
	 (result (list :prefix (plist-get info :prefix)
		       :candidates (ensime--annotate-completions
				    (plist-get info :completions)))))
    ;; Refresh the cache with latest from server.
    (setq ensime--completion-cache result)
    result))

(defun ensime--candidate-cache-is-valid (prefix)
  "Returns t if all completions for the given prefix have already been
 received from server."
  (and ensime--completion-cache
       (let ((cache-prefix (plist-get ensime--completion-cache :prefix))
	     (cache-count (length (plist-get ensime--completion-cache :candidates))))
	 (and (string-prefix-p cache-prefix prefix)
	      (< cache-count ensime-ac-max-results)))))

(defun ensime--get-completion-prefix-at-point ()
  "Returns the prefix to complete. TODO - must respect scala identifier syntax."
  (let ((text (buffer-substring-no-properties
	       (max 1 (- (point) 100)) (point)))
	(case-fold-search t))
    (if (string-match "\\([a-z0-9_-]+\\)\\'" text)
	(match-string 1 text)
      "")))

(defun ensime-company (command &optional arg &rest _args)
  "Ensime backend for company-mode."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'ensime-company))

    (`prefix (ensime--get-completion-prefix-at-point))

    (`candidates
     `(:async . ensime--ask-server-for-completions-async))

    ;; Don't do client-side sorting (preserve server-side rankings).
    (`sorted t)

    (`no-cache (not (ensime--candidate-cache-is-valid arg)))

    ;; We handle dup removal on the server.
    (`duplicates nil)

    ;; Show an inline signature for callable completions.
    (`annotation
     (when (get-text-property 0 'is-callable arg)
       (ensime-ac-brief-type-sig (get-text-property 0 'type-sig arg))))

    ;; Expand function formal parameters if we've completed a call.
    (`post-completion (ensime--yasnippet-complete-action arg))

    (`ignore-case t)
    (`require-match `never)
    (`doc-buffer nil) ;; TODO for docs!
    (`meta nil) ;; TODO for short docs!
    (`location nil) ;; TODO Maybe use at some point to link to definitions.
    (_ nil)
    ))

(defun ensime-completion-at-point-function ()
  "Standard Emacs 24+ completion function, handles completion-at-point requests.
 See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html"
  (let* ((prefix (ensime--get-completion-prefix-at-point))
	 (start (- (point) (length prefix)))
	 (end (point))
	 (props '(:annotation-function
		  (lambda (m)
		    (when (get-text-property 0 'is-callable m)
		      (ensime-ac-brief-type-sig
		       (get-text-property 0 'type-sig m))))
		  :exit-function
		  (lambda (m status)
		    (when (eq status 'finished)
		      (ensime-ac-complete-action m)))))
	 (completion-func
	  (lambda (prefix pred action)
	    (cond
	     ((eq action 'metadata)
	      '(metadata . ((display-sort-function . identity))))
	     (t
	      (complete-with-action
	       action (plist-get (ensime--ask-server-for-completions)
				 :candidates) prefix pred))))))
    `(,start ,end ,completion-func . ,props)))


(provide 'ensime-auto-complete)

;; Local Variables:
;; no-byte-compile: t
;; End:

