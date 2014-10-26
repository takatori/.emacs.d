;; (eval-after-load 'helm
;;   '(progn
;;      (require 'helm-config)
;;      (require 'helm-command)
;;      (require 'helm-misc)
;;      (setq
;;       helm-idle-delay 0
;;       helm-input-idle-delay 0
;;       helm-candidate-number-limit 1000
;;       helm-quick-update t
;;       helm-m-occur-idle-delay 0
;;       helm-ff-transformer-show-only-basename nil
;;       helm-split-window-preferred-function (lambda (window)
;;                                              (cond ((require 'popwin nil t)
;;                                                     (nth 1 (popwin:create-popup-window 25)))
;;                                                    (t
;;                                                     (split-window-sensibly)))))

;;      (require 'helm-project)            
;;      (defun helm-project-mini ()
;;        (interactive)
;;        (let ((file-list '(helm-source-buffers-list helm-source-recentf helm-source-find-files helm-c-source-buffer-not-found)))
;;          (if (and (projectile-project-root) (and
;;                                              (not (equal (projectile-project-root) "~/"))
;;                                              (not (equal (projectile-project-root) "/"))))
;;              (add-to-list 'file-list helm-source-project-files))
;;          (helm-other-buffer file-list "*helm for files*")))
;;      (global-set-key (kbd "M-o") 'helm-project-mini)
;;      (global-set-key (kbd "s-o") 'helm-project-mini)))

;; (global-set-key (kbd "M-s") 'helm-occur)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "M-z") 'helm-resume)
;; (global-set-key (kbd "s-o") 'helm-mini)

(require 'helm-config)

;; --------------------------------------------------------
;; key-bind
(global-set-key (kbd "C-c h") 'helm-mini)
;; 自動補完を無効
(custom-set-variables '(helm-ff-auto-update-initial-value nil))
;; C-hでバックスペースと同じように文字を削除  
(define-key helm-c-read-file-map (kbd "C-h") 'delete-backward-char)
;; TABで任意補完。選択肢が出てきたらC-nやC-pで上下移動してから決定することも可能
(define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)


(helm-mode 1)
(provide 'init-helm)
