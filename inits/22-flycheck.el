(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; エラー内容をツールチップで表示
;; @see https://github.com/flycheck/flycheck-pos-tip
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
