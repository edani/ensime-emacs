(setq debug-on-error t)
(add-hook 'kill-emacs-hook
          (lambda() (message "Exiting ENSIME tests at %s" (backtrace))))

(setq user-emacs-directory (expand-file-name "./emacs.d"))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(require 'cl)

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
	      (error (message "Failed to install %s at %s." p)))
	    ))))
  (error (message "Error loading dependencies: %s" err)))

(when (getenv "UNDERCOVER")
  (unless (package-installed-p 'undercover)
    (package-install 'undercover))
  (when (require 'undercover nil t)
    (undercover "ensime*.el"
                (:report-file "coveralls.json")
                (:send-report nil)
                (:exclude "ensime-test.el" "dotemacs_test.el" "ensime-inspector.el"))))

(add-to-list 'load-path "./")
(require 'ensime)
(require 'ensime-test)
(setq ensime-test-dev-home (expand-file-name "./"))
(setq ensime-log-events t)
(setq ensime-typecheck-when-idle nil)
(message "Using ensime-test-dev-home of %s" ensime-test-dev-home)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq inhibit-startup-message t)
(setq debug-on-quit nil)
(setq visible-bell t)
(setq visual-line-mode nil)
(setq mark-even-if-inactive t)
(show-paren-mode t)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(setq backup-directory-alist '(("." . (ensime-temp-directory))))
