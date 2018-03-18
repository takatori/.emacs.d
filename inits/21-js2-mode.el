;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . js2-mode))

;; セミコロンがなくても警告を出さないようにする
(setq-default js2-strict-missing-semi-warning nil)

(add-hook 'js2-mode-hook
          (lambda ()
             (setq my-js-mode-indent-num 2)
             (setq js2-basic-offset my-js-mode-indent-num)
             (setq js-switch-indent-offset my-js-mode-indent-num)))

