(projectile-global-mode)
(require 'helm-projectile)

;; enable caching
(setq projectile-enable-caching t)

;; Indexing methodを早い方にする Windows以外はdefaultでalien
;; 問題があれば nativeに変更する
(setq projectile-indexing-method 'alien)

;; プロジェクトに関連するファイルをhelm-for-filesに追加
(defadvice helm-for-files (around update-helm-list activate)
  (let ((helm-for-files-preferred-list
         (helm-for-files-update-list)))
    ad-do-it))

(defun helm-for-files-update-list ()
  `(helm-source-buffers-list
    helm-source-recentf
    helm-source-ghq
    helm-source-files-in-current-dir
    helm-source-file-cache
    ,(if (projectile-project-p)
     helm-source-projectile-files-list)))

;; C-c p pでprojectを移動した時のデフォルトアクションをdiredに変更
(setq projectile-switch-project-action 'projectile-dired)

;; helm-agをプロジェクトルートから
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))


