;; Goでの開発に必要なパッケージ・設定

;; 参考
;; http://emacs-jp.github.io/programming/golang.html

(require 'go-mode)
(require 'go-eldoc)
(require 'company-go)

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'flycheck-mode) ;; flycheck-modeを有効化してシンタックスエラーを検知
(add-hook 'go-mode-hook (lambda()
       (add-hook 'before-save-hook' 'gofmt-before-save)
       (local-set-key (kbd "M-.") 'godef-jump)
       (setq indent-tabs-mode nil)    ; タブを利用
       (setq tab-width 4)))

;; company-modeとの連携してコード補完する
(add-to-list 'company-backends 'company-go)

