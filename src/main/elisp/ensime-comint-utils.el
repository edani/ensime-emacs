;;; ensime-comint-utils.el
;;
;;;; License
;;
;;     Copyright (C) 2012 Grégoire Neuville
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

(require 'comint)

(defvar ensime-comint-completion-buffers ()
  "List of buffer names (strings) in which `ensime-comint-complete'
will be available (i.e will actually do something).")

(defvar ensime-comint-completion-invalid-values "\C-h\\|{invalid input}"
  "Regexp matching values to be discarded from
the output received after a call to `ensime-comint-complete'.")

(defun ensime-comint-sanitise(str)
  (replace-regexp-in-string
   ensime-comint-completion-invalid-values
   "" str))

(defun ensime-comint-shape-candidate (candidate cand-max-length nbr-cols candidates)
  (let* ((cand-length (length candidate))
         (cand-index (+ 1 (position candidate candidates :test 'string=)))
         (nbr-spaces (- cand-max-length cand-length))
         (new-cand (concat candidate (make-string nbr-spaces ? ))))
    (if (= 0 (% cand-index nbr-cols))
        (concat candidate "\n")
      (concat new-cand " "))))


(defun ensime-comint-shape-candidates (candidates)
  (let* ((wwidth (window-width))
         (cand-max-length
          (+ 1 (apply 'max (mapcar 'length candidates))))
         (nbr-cols (/ wwidth cand-max-length)))
    (mapcar '(lambda (cand)
               (ensime-comint-shape-candidate
                cand cand-max-length nbr-cols candidates))
            candidates)))

(defun ensime-comint-cplt-output-filter (output)
  (let* ((output-list (split-string (ensime-comint-sanitise output)))
         (proc (get-buffer-process (current-buffer)))
         (triggered-by-ensime-comint-cplt
          (process-get proc 'ensime-comint-completion)))
    (if (and triggered-by-ensime-comint-cplt output-list)
        (let ((input (first (reverse output-list)))
              (prompt (second (reverse output-list))))
          (cond
           ((equal output-list (list prompt input)) ;; only one candidate
            (process-put proc 'ensime-comint-completion nil) ;; deactivate filter
            (concat "\n" prompt " " input))
           ((and output-list (and input prompt)) ;; several candidates
            (process-put proc 'ensime-comint-completion nil) ;; deactivate filter
            (concat (mapconcat 'identity
                       (ensime-comint-shape-candidates
                        (reverse (set-difference
                                  output-list
                                  (list prompt input)))) "")
                    "\n" prompt " " input))
           (t ;; should not happen
            output)))
      output)))

(defun ensime-comint-complete ()
  "Get the completion candidates from sbt/repl process"
  (interactive)
  (if (member (buffer-name) ensime-comint-completion-buffers)
      (let* ((proc (get-buffer-process (current-buffer)))
             (input (buffer-substring (comint-line-beginning-position) (point))))
        (process-put proc 'ensime-comint-completion t) ;; activate ensime-comint-cplt-output-filter
        (comint-kill-input)
        (comint-proc-query proc (concat input (kbd "TAB")))
        (comint-proc-query proc (kbd "C-a"))
        (comint-proc-query proc (kbd "C-k"))
        (sit-for 0.2) ;; make sure output has been processed and rendered
        ;; the below is a bit silly but didn't find any other way to prevent
        ;; the completed word from becoming read-only
        (let ((new-input (buffer-substring (comint-line-beginning-position) (point)))
              (inhibit-read-only t))
          (kill-region (comint-line-beginning-position) (point))
          (remove-list-of-text-properties 0 (length new-input) '(read-only) new-input)
          (insert new-input)))))

(provide 'ensime-comint-utils)

