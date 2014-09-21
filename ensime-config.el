;;; ensime-config.el
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


(eval-and-compile (require 'ensime-macros))

(defvar ensime-config-file-name ".ensime"
  "The default file name for ensime project configurations.")

(add-to-list 'auto-mode-alist '("\\.ensime$" . emacs-lisp-mode))

(defmacro ensime-set-key (conf key val)
  `(setq ,conf (plist-put ,conf ,key ,val)))

(defun ensime-config-find-file (file-name)
  "Search up the directory tree starting at file-name
   for a suitable config file to load, return it's path. Return nil if
   no such file found."
  ;;(ensime-config-find-file "~/projects/ensime/")
  ;;(ensime-config-find-file "~/projects/ensime/src/main")
  ;;(ensime-config-find-file "~/projects/ensime/src/main/scala")
  ;;(ensime-config-find-file "~/projects/ensime/src/main/scala/")
  ;;(ensime-config-find-file "~/projects/ensime/.ensime")
  (let* ((dir (file-name-directory file-name))
	 (possible-path (concat dir ensime-config-file-name)))
    (when (and dir (file-directory-p dir))
      (if (file-exists-p possible-path)
	  possible-path
	(if (not (equal dir (directory-file-name dir)))
	    (ensime-config-find-file (directory-file-name dir)))))))

(defun ensime-config-find (&optional force-dir)
  "Query the user for the path to a config file, then load it."
  (let* ((hint (or force-dir buffer-file-name default-directory))
	 (guess (when hint (ensime-config-find-file hint)))
	 (file (if ensime-prefer-noninteractive
                   guess
		 (read-file-name
		  "ENSIME Project file: "
		  (if guess (file-name-directory guess))
		  guess
		  nil
		  (if guess (file-name-nondirectory guess) "")))))

    (if (and (file-exists-p file)
             (not (file-directory-p file)))
        file
      (warn (concat
              "Could not find an ENSIME project file. "
              "Please see the ENSIME guide: "
              "https://github.com/ensime/ensime-server/wiki/Quick-Start-Guide "
              "for instructions on how to write or "
              "generate a config file."))
      nil)))

(defun ensime-config-load (file-name &optional force-dir)
  "Load and parse a project config file. Return the resulting plist.
   The :root-dir setting will be deduced from the location of the project file."
  (let ((dir (expand-file-name (file-name-directory file-name)))
	(source-path (or force-dir buffer-file-name default-directory)))
    (save-excursion
      (let ((config
	     (let ((buf (find-file-read-only file-name ensime-config-file-name))
		   (src (buffer-substring-no-properties
			 (point-min) (point-max))))
	       (kill-buffer buf)
	       (condition-case error
		   (read src)
		 (error
		  (error "Error reading configuration file, %s: %s" src error)
		  ))
	       )))
	;; We use the project file's location as the project root.
	(ensime-set-key config :root-dir dir)
        (ensime-set-key config
                        :source-jars-dir
                        (file-name-as-directory
                         (concat
                          dir
                          (file-name-as-directory ".ensime_cache/dep-src")
                          (file-name-as-directory "source-jars"))))
	(ensime-config-maybe-set-active-subproject config source-path)
	config)
      )))

(defun ensime-config-maybe-set-active-subproject (config &optional source-path)
  "If the subprojects key exists in the config, prompt the
 user for the desired subproject, and add an active-subproject
 value to the config."
  (when-let (sps (plist-get config :subprojects))

    ;; For testing purposes..
    (if (or ensime-prefer-noninteractive
	    (= (length sps) 1))
	(ensime-set-key
	 config :active-subproject
	 (plist-get (car sps) :module-name))

      ;; Otherwise prompt the user
      (let ((keys (ensime-config-candidate-subprojects config source-path)))
	(when-let (chosen (when keys
		     (completing-read
		      (concat "Which project? ("
			      (mapconcat #'identity keys ", ")
			      "): ")
		      keys nil t (car keys))))
           (ensime-set-key config :active-subproject chosen)
	    ))
	)))

(defun ensime-config-get-activeproject (full-config)
  "Returns the configuration plist for the active-subproject. If
 :active-subproject has not been set then the original full-config
 is returned"
  (let ((active-name (plist-get full-config :active-subproject))
        (sps (plist-get full-config :subprojects)))
    (flet ((match (a b) (equal a (plist-get b :module-name))))
      (if active-name
        (find active-name sps :test 'match)
        full-config)
      )))

(defun ensime-config-candidate-subprojects (config source-path)
  (catch 'return
    (let ((all-subprojects
           (sort
            (mapcar (lambda (sp) (plist-get sp :module-name))
                    (plist-get config :subprojects))
             'string<)))

      (unless source-path
        (throw 'return all-subprojects))

      (dolist (d (plist-get config :source-roots))
        (when (ensime-file-in-directory-p source-path d)
          (throw 'return all-subprojects)))

      (let ((all-deps (ensime-config-transitive-dependencies config))
            (modules-by-name (make-hash-table :test 'equal))
            (result nil))
        (dolist (sp (plist-get config :subprojects))
          (let ((m (plist-get sp :module-name)))
            (puthash m sp modules-by-name)))

        (dolist (module-and-deps all-deps)
          (dolist (m module-and-deps)
            (let* ((sp (gethash m modules-by-name))
                   (source-dirs (plist-get sp :source-roots)))
              (dolist (d source-dirs)
                (when (ensime-file-in-directory-p source-path d)
                  (push (first module-and-deps) result))))))

        (if result
            (sort (remove-duplicates result :test 'equal) 'string<)
          all-subprojects)))))

(defun ensime-config-transitive-dependencies (config)
  (let ((names nil)
        (all-deps (make-hash-table :test 'equal)))
    (dolist (sp (plist-get config :subprojects))
      (let ((m (plist-get sp :module-name))
            (deps (plist-get sp :depends-on-modules)))
        (push m names)
        (dolist (d deps) (puthash (cons m d) 1 all-deps))))
    ;; Warshal's transitive closure algorithm
    (dolist (k names)
      (dolist (i names)
        (dolist (j names)
          (when (and (gethash (cons i k) all-deps)
                     (gethash (cons k j) all-deps))
            (puthash (cons i j) 1 all-deps)))))

    (let ((response nil))
      (dolist (i names)
        (let ((deps nil))
          (dolist (j names)
            (when (gethash (cons i j) all-deps)
              (push j deps)))
          (push (cons i deps) response)))
      response)))

(provide 'ensime-config)

;; Local Variables:
;; no-byte-compile: t
;; End:

