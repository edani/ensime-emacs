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
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun ensime--install-dependency (dependency)
  "PKG-INFO: List with package as the head and version as the tail."
  (let ((pkg (car dependency))
        (version (cadr dependency)))
    (if (package-installed-p pkg (version-to-list version))
        (message "%s is already installed" pkg)
      (package-install pkg))))

;; Load all package dependencies
(let* ((pkg-info
        (with-temp-buffer
          (insert-file-contents "ensime-pkg.el")
          (goto-char (point-min))
          (read (current-buffer))))
       (name (cadr pkg-info))
       (needed-packages (cadr (nth 4 pkg-info))))
  (message "Installing dependencies: %S" needed-packages)
  (dolist (dependency needed-packages nil)
    (ensime--install-dependency dependency)))

;; enable coverage
(when (getenv "UNDERCOVER")
  (ensime--install-dependency '(undercover "0.5.0"))
  (require 'undercover)
  (undercover "ensime*.el"
              (:report-file (expand-file-name "./coveralls.json"))
              (:send-report nil)
              (:exclude "ensime-test.el" "dotemacs_test.el" "ensime-inspector.el")))

(add-to-list 'load-path (expand-file-name "./"))
(require 'ensime)
(require 'ensime-test)
(setq ensime-server-logback (concat ensime-test-dev-home "/test/logback.xml"))

(message "Using ensime-test-dev-home of %s" ensime-test-dev-home)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; dotemacs_test.el ends here
