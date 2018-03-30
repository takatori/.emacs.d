;; apib-mode
;; https://github.com/w-vi/apib-mode
(autoload 'apib-mode "apib-mode"
        "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; drafterも入れましょう
;; api blue print parser
;; https://github.com/apiaryio/drafter
;; OS Xの場合は以下でインストール
;; $ brew install --HEAD \
;;   https://raw.github.com/apiaryio/drafter/master/tools/homebrew/drafter.rb
