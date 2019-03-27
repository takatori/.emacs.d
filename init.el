
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alst nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;;; Functions

(eval-and-compile
  
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory))
  
  (defun lookup-password (host user port)
    (require 'auth-source)
    (require 'auth-source-pass)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s user host port"))))
  
  (defvar saved-window-configuration nil)
  
  (defun push-window-configuration ()
    (interactive)
    (push (current-window-configuration) saved-window-configuration))
  
  (defun pop-window-configuration ()
    (interactive)
    (let ((config (pop saved-window-configuration)))
      (if config
          (set-window-configuration config)
        (if (> (length (window-list)) 1)
            (delete-window)
          (bury-bufffer))))))


;;; Environment

(eval-and-compile
  (defconst emacs-environment (getenv "NIX_MYENV_NAME"))

  (setq load-path
	(append '("~/.emacs.d")
		(delete-dups load-path)
		'("~/.emacs.d/lisp")))

  (defun filter (f args)
    (let (result)
      (dolist (arg args)
	(when (funcall f arg)
	  (setq result (cons arg result))))
      (nreverse result)))

  

  (defconst load-path-reject-re "/\\.emacs\\.d/\\(lib\\|site-lisp\\)/"
    "Regexp matching `:load-path' values to be rejected.")

  (defun load-path-handler-override (orig-func name keyword args rest state)
    (if (cl-some (apply-partially #'string-match load-path-reject-re) args)
	(use-package-process-keywords name rest state)
      (let ((body (use-package-process-keywords name rest state)))
	(use-package-concat
	 (mapcar #'(lambda (path)
		     `(eval-and-compile (add-to-list 'load-path ,path t)))
		 args)
	 body))))

  (advice-add 'use-package-handler/:load-path
	      :around #'load-path-handler-override)

  (if init-file-debug
      (setq use-package-verbose t
	    use-package-expand-minimally nil
	    use-package-compute-statistics t
	    debug-on-error t)
    (setq use-package-verbose nil
	  use-package-expand-minimally t)))

;;; Settings

(setq custom-file "~/.emacs.d/settings.el")
(load (emacs-path "settings"))
