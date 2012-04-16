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

(defvar ensime-comint-filter-buffer " *ensime-comint-filter-buffer*"
  "Name of the buffer used by `ensime-comint-cplt-output-filter'
to put output from process into for further processing.")

(defvar ensime-comint-completion-invalid-values "\C-h"
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

(defun ensime-comint-treat-output (output)
  (let* ((output-list
          (cdr (split-string (ensime-comint-sanitise output))))
         (input (first (reverse output-list)))
         (prompt (second (reverse output-list))))
          (cond
           ((equal output-list (list prompt input)) ;; only one candidate
            (concat "\n" prompt " " input))
           ((and output-list (and input prompt)) ;; several candidates
            (concat (mapconcat 'identity
                       (ensime-comint-shape-candidates
                        (reverse (set-difference
                                  output-list
                                  (list prompt input)))) "")
                    "\n" prompt " " input))
           (t ;; no candidates
            ""))))

(defun ensime-comint-cplt-output-filter (output)
  (let* ((proc (get-buffer-process (current-buffer)))
         (triggered-by-ensime-comint-cplt
          (process-get proc 'ensime-comint-completion)))
    (if triggered-by-ensime-comint-cplt
        (progn
          (with-current-buffer (get-buffer-create ensime-comint-filter-buffer)
            (insert output))
          "")
      output)))

(defun ensime-comint-complete ()
  "Get the completion candidates from sbt/repl process"
  (interactive)
  (if (member (buffer-name) ensime-comint-completion-buffers)
      (let* ((proc (get-buffer-process (current-buffer)))
             (input (buffer-substring (comint-line-beginning-position) (point))))
        (if (string-to-list input)
            (progn
              (process-put proc 'ensime-comint-completion t) ;; activate ensime-comint-cplt-output-filter
              (comint-proc-query proc (concat input (kbd "TAB")))
              (comint-proc-query proc (kbd "C-a"))
              (comint-proc-query proc (kbd "C-k"))
              (sit-for 0.2) ;; make sure all output has been  received
              (process-put proc 'ensime-comint-completion nil) ;; deactivate filter
              (let ((custom-output (with-current-buffer
                                     (get-buffer-create ensime-comint-filter-buffer)
                                   (ensime-comint-treat-output (buffer-string)))))
                (if (string-to-list custom-output)
                    (progn
                      (comint-kill-input)
                      (comint-output-filter proc (concat "\n" custom-output))
                      ;; the below is a bit silly but I didn't find any other way to prevent
                      ;; the completed word from becoming read-only
                      (let ((new-input (buffer-substring
                                        (comint-line-beginning-position)
                                        (point)))
                            (inhibit-read-only t))
                        (kill-region (comint-line-beginning-position) (point))
                        (remove-list-of-text-properties 0 (length new-input) '(read-only) new-input)
                        (insert new-input)))
                  (message "No completion candidates")))
              (kill-buffer ensime-comint-filter-buffer))
          (message "At least one character must be entered !")))))

(provide 'ensime-comint-utils)
