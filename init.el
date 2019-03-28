
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

  ;; init straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; install use-package
  (straight-use-package 'use-package)

  ;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
  ;; 本来は (use-package hoge :straight t) のように書く必要がある
  (setq straight-use-package-by-default t)
  
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

;;; Keymaps

(eval-and-compile
  (mapc #'(lambda (entry)
	    (define-prefix-command (cdr entry))
	    (bind-key (car entry) (cdr entry)))
	    '(("C-l" . kill-ring-save)
	      )))

;;; Packages

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
 '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
 '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))


(use-package expand-region
  :bind ("C-," . er/expand-region))
