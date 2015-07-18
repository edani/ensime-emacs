(setq debug-on-error t
      debug-on-quit t
      ensime-log-events t ;; how can we see this in batch mode?
      ensime-typecheck-when-idle nil
      user-emacs-directory (expand-file-name "./emacs.d")
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(add-hook 'kill-emacs-hook
          (lambda() (message "Exiting ENSIME tests at %s" (backtrace))))

(require 'package)
(require 'cl)

(package-initialize)

(defun pkg-installed-p (pkg)
  (package-installed-p (car pkg) (version-to-list (cadr pkg))))

;; Load all package dependencies
(condition-case err
    (let* ((pkg-info
	    (with-temp-buffer
	      (insert-file-contents "ensime-pkg.el")
	      (goto-char (point-min))
	      (read (current-buffer))))
	   (name (cadr pkg-info))
	   (needed-packages (cadr (nth 4 pkg-info))))
      (assert (equal name "ensime"))
      (message "Loaded ensime-pkg.el")
      (message "Installing dependencies: %S" needed-packages)
      (if (every #'pkg-installed-p needed-packages)
	  (message "All dependencies present.")
	(package-refresh-contents)
	(dolist (p needed-packages)
	  (unless (pkg-installed-p p)
	    (package-install (car p))
	    (when (not (pkg-installed-p p))
	      (error (message "Failed to install %s." p)))))))
  (error (message "Error loading dependencies: %s" err)))

;; enable coverage
(when (getenv "UNDERCOVER")
  (unless (package-installed-p 'undercover)
    (package-install 'undercover))
  (when (require 'undercover nil t)
    (undercover "ensime*.el"
                (:report-file "coveralls.json")
                (:send-report nil)
                (:exclude "ensime-test.el" "dotemacs_test.el" "ensime-inspector.el"))))

(add-to-list 'load-path (expand-file-name "./"))
(require 'ensime)
(require 'ensime-test)

(message "Using ensime-test-dev-home of %s" ensime-test-dev-home)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;(setq backup-directory-alist '(("." . (ensime-temp-directory))))
