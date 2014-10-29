(require 'projectile)

(defvar helm-source-project-files
  '((name . "helm project files")
    (init . (lambda ()
              (helm-candidate-buffer (project-files-to-buf (project-root-or-current-directory)))))
    (candidates-in-buffer)
    (display-to-real . (lambda (display)
                         (with-current-buffer helm-current-buffer
                           (format "%s%s" (projectile-project-root) (car (split-string display " => "))))))
    (action .  (("Find file" . helm-find-many-files)
                ("View file" . view-file)
                ("Insert file" . insert-file)
                ("Delete file(s)" . helm-delete-marked-files)))))

(defun project-files-to-buf (directory)
  (let ((result-buf (get-buffer-create (format "*ack-files-%s*" directory)))
        (process-name (format "ack-files-%s" directory)))
    (with-current-buffer result-buf (erase-buffer))
    (set-process-sentinel
     (let ((default-directory directory))
       (start-process process-name result-buf "ack" "-f"))
     (lambda (proc stat)
       (with-current-buffer (process-buffer proc)
         (while (re-search-forward "Process .* finished" nil t)
           (beginning-of-line)
           (kill-line)))))
    (run-at-time "10 sec"
                 nil
                 (lambda (process-name)
                   (cond ((get-process process-name)
                          (kill-process process-name)
                          (message "killed long running process: %s" process-name))))
                 process-name)
    result-buf))

(defun project-root-or-current-directory ()
  (or (projectile-project-root) default-directory))

(defun helm-search-project-files (root-directory patterns)
  (let* ((default-directory root-directory)
         (content-patterns (-map (lambda (pattern) (substring pattern 1)) (-filter (lambda (pattern) (s-starts-with? "@" pattern)) patterns)))
         (content-patterns-regexp )
         (file-patterns (-remove (lambda (pattern) (s-starts-with? "@" pattern)) patterns))
         (command (format "ag --nocolor --nogroup --smart-case '%s' -G '%s' | head" (build-regexp-from-patterns content-patterns ".") (build-regexp-from-patterns file-patterns "."))))
    ;; (message "root-directory: %s\npatterns: %s\ncontent-patterns: %s\nfile-patterns: %s\ncommand: %s" root-directory patterns content-patterns file-patterns command)
    (split-string (shell-command-to-string command) "\n")))

(defun helm-search-project-files-match (candidate)
  t)

(defvar helm-source-search-project-cache (make-hash-table :test 'equal))
(defvar helm-source-search-project
  '((name . "helm search project")
    (requires-pattern)
    (volatile)
    (recenter)
    (delayed)
    (match helm-search-project-files-match)
    (init . (lambda ()
              (setq
               helm-source-search-project-root (project-root-or-current-directory))
              (puthash helm-source-search-project-root (make-hash-table :test 'equal) helm-source-search-project-cache)))
    (candidates . (lambda ()
                    (let ((ack-project-results-cache (gethash helm-source-search-project-root helm-source-search-project-cache)))
                      (unless (gethash helm-pattern ack-project-results-cache)
                        (puthash helm-pattern (helm-search-project-files helm-source-search-project-root (split-string helm-pattern)) ack-project-results-cache))
                      (gethash helm-pattern ack-project-results-cache))))
    (action . (("Go to" . helm-goto-project-file-line)))
    (type . file-line)))

(defun helm-goto-project-file-line (file-line)
  (with-current-buffer helm-current-buffer
    (let* ((current-directory (file-name-directory (or (buffer-file-name) dired-directory)))
           (file (nth 1 (split-string (nth 2 file-line) current-directory)))
           (lineno (nth 0 file-line))
           (content (nth 1 file-line)))
      ;; (message (format "file-line: %s\ncurrent-directory: %s\nfile: %s\nlineno: %s\ncontent: %s" file-line current-directory file lineno content))
      (helm-goto-file-line lineno content (format "%s%s" helm-source-search-project-root file)))))

(defun helm-search-project ()
  (interactive)
  (helm-other-buffer '(helm-source-search-project)
                     (format "*helm project files (%s)*" (project-root-or-current-directory))))

(defun helm-do-grep-project ()
  (interactive)
  (helm-do-grep-1 (list (project-root-or-current-directory)) t))

(global-set-key (kbd "s-f") 'helm-search-project)

(provide 'helm-project)
